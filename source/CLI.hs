{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

CLI implementation & entry point.

|-}
module Main where

import Prelude hiding                        (readFile, writeFile)

import Control.Exception

import qualified Data.HashMap.Strict as HashMap

import qualified Data.Text as Text

import Data.Encoding.Exception               (DecodingException)
import Data.Encoding.CP1252

import Text.Megaparsec hiding                (count)

import System.Exit                           (exitFailure)
import System.FilePath                       ((</>), takeDirectory, takeFileName, takeExtension)
import System.Directory                      (doesPathExist, doesDirectoryExist
                                             , listDirectory
                                             , createDirectoryIfMissing, renameDirectory)
import System.IO.Temp                        (withTempDirectory)
import System.IO.Encoding                    (readFile, writeFile)

import qualified Options.Applicative as Args
import qualified Options.Applicative.Help as Args hiding (fullDesc)

import Control.Concurrent.Async
import System.Console.Concurrent
import System.Console.Regions

import qualified Hardcoded
import Types
import Traits
import Localisation.Base
import Localisation
import Oob

noteBriefly :: Text -> IO ()
noteBriefly = outputConcurrent

note :: Text -> IO ()
note = noteBriefly . (<> "\n")

note' :: [Text] -> IO ()
note' = note . mconcat

noteFailure :: Text -> IO noreturn
noteFailure message = note message *> exitFailure

-- | Type restriction for better inference.
consoleRegion :: (LiftRegion m) => ConsoleRegion -> Text -> m ()
consoleRegion = setConsoleRegion

-- | Type restriction for better inference.
append :: (LiftRegion m) => ConsoleRegion -> Text -> m ()
append = appendConsoleRegion

-- | Like `finishConsoleRegion`, but append to current region contents.
concludeWith :: (LiftRegion m) => ConsoleRegion -> Text -> m ()
concludeWith region suffix = liftRegion $ do
    contents <- getConsoleRegion region

    -- !! this sequencing is necessary !!
    -- Though there is nothing in the documention of concurrent-output to suggest it is required, we
    -- get intermitteng segfaults if we don’t force this concatenation result ahead of
    -- `finishConsoleRegion`.
    let !finisher = contents <> suffix

    finishConsoleRegion region finisher

-- | Like `finishConsoleRegion`, but preserve current region contents.
conclude :: (LiftRegion m) => ConsoleRegion -> m ()
conclude region = liftRegion $ finishConsoleRegion region =<< getConsoleRegion region

filePath :: FilePath -> Text
filePath = Text.pack

-- | Read a game or mod file. Uses the CP1252 encoding that Victoria II expects.
fileContents :: FilePath -> IO (Either DecodingException Text)
fileContents path = handle decodingError $ do
    let ?enc = CP1252
    -- N.b., System.IO.Encoding functions report decoding errors as pure exceptions
    --         vvvvvvvv
    unicode <- evaluate =<< readFile path
    pure $ pure $ Text.pack unicode

  where
    decodingError :: DecodingException -> IO (Either DecodingException Text)
    decodingError err = pure $ Left err

decodingFailure :: Text -> FilePath -> DecodingException -> IO noreturn
decodingFailure what path err = noteFailure [iTrim|
Decoding of ${what} ‘${ filePath path }’ failed, aborting:
    ${err}
|]

prettyParseErrors :: ParseErrorBundle Text Void -> Text
prettyParseErrors = Text.pack . errorBundlePretty

localisationContents :: ConsoleRegion -> BaseGameLocalisation -> FilePath -> FilePath -> IO Text
localisationContents region baseGameLocalisation base path = do
    contents <- fileContents path
    case contents of
        Left err -> do
            -- see `Localisation.Base`
            if takeFileName base == "Victoria 2"
                && ("localisation" </> "text.csv") `isSuffixOf` path
                then if baseGameLocalisation == IncludeBaseGame
                         then append region [i|
Decoding of localisation file failed:
    ‘${ filePath path }’: ${err}
But it looks like it is base game ‘text.csv’ file, which translations are already incorporated into
this program. Continuing.
|]
                         else append region [i|
Decoding of localisation file failed:
    ‘${ filePath path }’: ${err}
But it looks like it is base game ‘text.csv’ file. If you want to use base game localisation,
consider using `--include-base-game` (refer to `--help`). Continuing after skipping the file.
|]
                else append region [i|
Decoding of one localisation file failed, skipping it:
    ‘${ filePath path }’: ${err}|]

            -- We get away with this because an empty localisation file is valid and corresponds
            -- to no entry at all.
            pure ""

        Right decoded -> pure decoded

parseArgs :: IO (FilePath, [FilePath], BaseGameLocalisation)
parseArgs = Args.execParser $ Args.info (Args.helper <*> args) desc
  where
    args = (,,) <$> modPath <*> extraLoc <*> (includeBaseGame <|> noIncludeBaseGame)

    paragraphs       = Args.unChunk . Args.vcatChunks
    separate   items = fmap (\item -> paragraph item `cat` blank) items
      where
        cat = Args.chunked (Args..$.)

    help     = Args.helpDoc     . paragraphs . separate
    progDesc = Args.progDescDoc . paragraphs

    paragraph  = Args.paragraph
    blank :: (Applicative f, Monoid a) => f a
    blank = pure mempty -- ^ blank line acting as separator

    modPath = Args.strArgument $
        help [ [iTrim|
Base path to the mod of interest. Contents at that location should conform to the usual structure
for mod files, so that the traits file (‘common/traits.txt’) as well as localisation information can
be found. (If the mod is not yet providing a traits file, you should copy over the unmodded traits
file.)
|]
             , [iTrim|
The current working directory is assumed if the path is not provided. All files will be assumed to
be using the WINDOWS-1252 encoding that the game expects.
|]
             ]
        <> Args.metavar "PATH/TO/MOD"
        <> Args.value "."

    extraLoc = Args.many $ Args.strOption $
        Args.long "extra-localisation"
        <> Args.short 'x'
        <> help [ [iTrim|
Extra base paths. The translations they contain will be collated for the purpose of computing the
final localisation file.
|]
                , [iTrim|
Order is significant: entries that come from a path that is passed in early are favoured over those
that come after. (Though extra paths are never processed before the base path of interest.)
|]
                , [iTrim|
You do NOT need to pass in the path to the game. Base game translations are already incorporated
into this program. (But also see: --no-include-base-game.)
|]
                ]
        <> Args.metavar "another/base/path"

    includeBaseGame = Args.flag IncludeBaseGame IncludeBaseGame $
        Args.long "include-base-game"
        <> help [ [iTrim|
Include the base game translations when computing the localisation file. This is the default
behaviour because base game file ‘localisation/text.csv’ is tricky to decode.
|]
                , [iTrim|
Base game translations are always considered last so that any other localisation you pass to the
program through `--extra-localisation` will be preferred.
|]
                , [iTrim|
(This has no effect on the computed traits, only on their translations.)
|]
                ]

    noIncludeBaseGame = Args.flag' NoIncludeBaseGame $
        Args.long "no-include-base-game"
        <> help [ [iTrim|
Do NOT include the base game translations when computing the localisation file. This suppresses the
default behaviour, see `--include-base-game`.
|]
                ]

    desc =
        Args.fullDesc
        <> progDesc [ blank
                    , paragraph [iTrim|
Linearise the personalities and backgrounds that generals and admirals can have. All output is
written to the ‘${ Hardcoded.outputBase }’ directory as long as it doesn’t exist (otherwise the
process is aborted), and consists of the following:
|]
                    , blank
                    , paragraph [iTrim|
- a `common/traits.txt` file containing linearised traits
|]
                    , paragraph [iTrim|
- a `localisation/linearised-traits.csv` file containing localisation entries
|]
                    , paragraph [iTrim|
- a `history/units` directory containing order-of-battle information modified to define leaders
  using the newly linearised traits
|]
                    , blank
                    , paragraph [iTrim|
Translations in the localisation file are collated from each of the base paths passed as arguments.
As such a localisation entry for each trait is required to be present if you want complete a
linearised localisation to be produced. Refer to the `--extra-localisation` option.
|]
                    ]

main :: IO ()
main = displayConsoleRegions $ do
    (modPath, extraPaths, baseGameLocalisation) <- parseArgs

    let traitsPath = modPath </> "common" </> "traits.txt"
        -- N.b. order is significant, see below.
        paths      = modPath:extraPaths

        localisationPath base      = base </> "localisation"
        localisationFile base path = localisationPath base </> path

        oobBase = modPath </> "history" </> "units"

    traitsContent <- either (decodingFailure "traits files" traitsPath) pure =<< fileContents traitsPath

    outputUnavailable <- doesPathExist Hardcoded.outputBase
    when outputUnavailable $ do
        noteFailure [iTrim|
Output destination ‘${ filePath Hardcoded.outputBase }’ already exists, aborting.
|]

    when (lineariserHeader `Text.isPrefixOf` traitsContent) $ do
        noteFailure [iTrim|
Traits file ‘${ filePath traitsPath }’ seems to already have been auto-generated by this program,
aborting. If you still want to attempt to linearise the file, remove the auto-generation header:

${ lineariserHeader }
|]

    let parseTraits traitsRegion = do
            consoleRegion traitsRegion [i|Adding traits from mod file ‘${ filePath traitsPath }’…|]
            case runParser traitsStructure traitsPath traitsContent of
                Left errs -> do
                    noteFailure [iTrim|
Parsing of traits file failed:

${ prettyParseErrors errs }
|]

                Right traits -> do
                    let personalities = length $ _traitsPersonalities traits
                        backgrounds   = length $ _traitsBackgrounds traits

                    for_ [ ("personality", personalities)
                        , ("background", backgrounds)
                        ] $ \(kind, count) -> when (count == 1) $ do
                            noteFailure [iTrim|
Nothing to linearise as there is only one ${kind::Text}, aborting. (Did you run the program on an
already linearised trait file?)
|]

                    concludeWith traitsRegion $ "\n    " <> [iTrim|
…found ${personalities} personalities and ${backgrounds} backgrounds, linearising to ${
personalities * backgrounds } composite traits.
|]

                    pure traits

        parseLocalisation localisationRegion =
            fmap (HashMap.unions . concat) $ do
                consoleRegion localisationRegion [iTrim|
Reading localisation from ${ length paths } base path(s)…
|]

                forConcurrently paths $ \base -> do
                    let pickCSVs = filter csvExtension
                        csvExtension = (== ".csv") . takeExtension

                    -- N.b. order is very significant. The key that appears in the first file in
                    -- lexical[1] order is the one that sets the translation, subsequent ones are
                    -- redundant.
                    --
                    -- [1]: presumably, at any rate, since this has not been tested in depth
                    locFiles <- (sort . pickCSVs) <$> listDirectory (localisationPath base)

                    append localisationRegion $ "\n    " <> [iTrim|
…adding ${ length locFiles } localisation files from base path ‘${ filePath base }’…
|]

                    forConcurrently locFiles $ \path -> do
                        contents <-
                            localisationContents localisationRegion baseGameLocalisation base $
                                localisationFile base path
                        case runParser localisations path contents of
                            Left errs -> do
                                append localisationRegion [i|
Parsing of one localisation file failed, skipping it:
    ${ prettyParseErrors errs }
|]

                                pure mempty

                            Right keys -> do
                                pure keys

        parseOob oobRegion = do
            consoleRegion oobRegion [iTrim|
Adding order-of-battle files from mod path ‘${ filePath oobBase }’…
|]

            oobListing <- listDirectory oobBase
            let partitionM pred = foldrM (selectM pred) (mempty, mempty)
                selectM pred item ~(accepted, rejected) = do
                    ok <- pred item
                    pure $ if ok then (item:accepted, rejected) else (accepted, item:rejected)

                parseOobFile target = do
                    let path = oobBase </> target
                    contents <- either (decodingFailure "order-of-battle file" path) pure =<< fileContents path
                    case runParser oob path contents of
                        Left errs -> do
                            if takeFileName path == "v2dd2.txt"
                                then do
                                    -- The game ships with a dev diary file inside history/units, we
                                    -- silently skip it.
                                    pure []

                                else noteFailure [iTrim|
Parsing of OOB file ‘${ filePath path }’ failed:

${ prettyParseErrors errs }
|]

                        Right (Right parsed) -> pure [(target, parsed)]

                        _ -> pure mempty

                oobPath listing = oobBase </> listing
                isBookmark = doesDirectoryExist . oobPath
                isOobFile  = (== ".txt") . takeExtension
                targetPath bookmark = fmap (bookmark </>) . filter isOobFile
                listOobFiles bookmark = targetPath bookmark <$> listDirectory (oobPath bookmark)

            (bookmarks, oobFiles') <- partitionM isBookmark oobListing
            bookmarkFiles <- mconcat <$> traverse listOobFiles bookmarks
            let oobFiles = targetPath "." oobFiles' <> bookmarkFiles
            oobEntries <- mconcat <$> mapM parseOobFile oobFiles

            concludeWith oobRegion $ "\n    " <> mconcat
                [ [i|…found ${ length oobFiles } order-of-battle files across|]
                , [i| ${ succ $ length bookmarks } start date(s), ${ length oobEntries } of which|]
                , [i| contain leader definitions.|]
                ]

            pure oobEntries

    let initRegion = openConsoleRegion Linear
    traitsRegion       <- initRegion
    localisationRegion <- initRegion
    oobRegion          <- initRegion

    ((traits, localisation), oobs) <-
        parseTraits traitsRegion
            `concurrently` parseLocalisation localisationRegion
            `concurrently` parseOob oobRegion

    case baseGameLocalisation of
        IncludeBaseGame   -> append localisationRegion $ "\n" <> mconcat
            [ [i|Found ${ HashMap.size localisation } entries across all localisation files,|]
            , [i| and adding to it ${ HashMap.size baseLocalisation } base game entries.|]
            ]
        NoIncludeBaseGame -> append localisationRegion [i|
Found ${ HashMap.size localisation } entries across all localisation files.|]

    conclude localisationRegion

    -- Compute everything!…
    let extendedLocalisation              = extendLocalisation baseGameLocalisation localisation
        linearisedTraits                  = lineariseTraits traits
        linearisedMods                    = lineariseMods linearisedTraits
        (personalities, backgrounds)      = traitsLocalisationKeys traits
        (orphans, linearisedLocalisation) =
            lineariseLocalisation linearisedMods extendedLocalisation personalities backgrounds
        linearisedOobs = oobs & mapped . _2 %~ lineariseOob

    when (not $ null orphans) $ do
        note [iTrim|
The following traits were missing a translation:
    ${ Text.intercalate ", " $ toList orphans }
(If you did not expect this, you might want to use `--help` and read about `--extra-localisation`
and/or `--include-base-game`.)
|]

    -- …and then write it out.
    withTempDirectory "." Hardcoded.outputBase $ \outputPath -> do
        let ?enc = CP1252
        let write path contents = do
                let target = outputPath </> path
                createDirectoryIfMissing {-- create parents --} True $ takeDirectory target
                writeFile target $ Text.unpack contents

        write Hardcoded.traitsOutput       . formatTraits       $ linearisedTraits
        write Hardcoded.localisationOutput . formatLocalisation $ linearisedLocalisation
        forM_ linearisedOobs $ \(target, oob) -> do
            write (Hardcoded.oobOutput target) $ formatOob oob

        renameDirectory outputPath Hardcoded.outputBase

    note [i|Done!|]
