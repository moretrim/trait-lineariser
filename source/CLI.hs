{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

CLI implementation & entry point.

|-}
module Main where

import Prelude hiding                        (getContents, readFile, writeFile)

import Control.Exception

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Traversable
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap

import Data.String.Here.Interpolated
import Data.String.Here.Uninterpolated
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Encoding.Exception               (DecodingException)
import Data.Encoding.UTF8
import Data.Encoding.CP1252

import Text.Megaparsec

import System.Exit                           (exitFailure)
import System.FilePath                       ((</>), takeFileName, takeExtension)
import System.Directory                      (doesPathExist, listDirectory, renameDirectory)
import System.IO.Temp                        (withTempDirectory)
import System.IO.Encoding                    (getContents, readFile, writeFile)

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

noteBriefly :: String -> IO ()
noteBriefly = outputConcurrent

note :: String -> IO ()
note = noteBriefly . (<> "\n")

note' :: [String] -> IO ()
note' = note . mconcat

noteFailure :: String -> IO noreturn
noteFailure message = note message *> exitFailure

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

localisationContents :: BaseGameLocalisation -> FilePath -> FilePath -> IO Text
localisationContents baseGameLocalisation base path = do
    contents <- fileContents path
    case contents of
        Left err -> do
            -- see `Localisation.Base`
            if takeFileName base == "Victoria 2"
                && ("localisation" </> "text.csv") `isSuffixOf` path
                then if baseGameLocalisation == IncludeBaseGame
                         then note [iTrim|
Decoding of localisation file failed:
    ‘${ path }’: ${err}
But it looks like it is base game ‘text.csv’ file, which translations are already incorporated into
this program. Continuing.
|]
                         else note [iTrim|
Decoding of localisation file failed:
    ‘${ path }’: ${err}
But it looks like it is base game ‘text.csv’ file. If you want to use base game localisation,
consider using `--include-base-game` (refer to `--help`). Continuing after skipping the file.
|]
                else note [iTrim|
Decoding of one localisation file failed, skipping it:
    ‘${ path }’: ${err}
|]

            -- We get away with this because an empty localisation file is valid and corresponds
            -- to no entry at all.
            pure ""

        Right decoded -> pure decoded

parseArgs :: IO (FilePath, [FilePath], BaseGameLocalisation)
parseArgs = Args.execParser $ Args.info (Args.helper <*> args) desc
  where
    args = (,,) <$> modPath <*> extraLoc <*> (includeBaseGame <|> noIncludeBaseGame)

    paragraphs       = Args.unChunk . Args.vcatChunks
    separate   items = fmap (\item -> para item `cat` blank) items
      where
        cat = Args.chunked (Args..$.)

    help     = Args.helpDoc     . paragraphs . separate
    progDesc = Args.progDescDoc . paragraphs

    para  = Args.paragraph
    blank :: (Applicative f, Monoid a) => f a
    blank = pure mempty -- ^ blank line acting as separator

    modPath = Args.strArgument $
        help [ [iTrim|
Base path to the mod of interest. Contents at that location should conform to the usual structure
for mod files, so that the traits file (`common/traits.txt`) as well as localisation information can
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
that come after.
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
behaviour because base game file `localisation/text.csv` is tricky to decode.
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
                    , para [iTrim|
Linearise the personalities and backgrounds that generals and admirals can have. All output is
written to the ‘${ Hardcoded.outputBase }’ directory as long as it doesn’t exist (otherwise the
process is aborted), and consists of the following:
|]
                    , blank
                    , para [iTrim|
- a `traits.txt` file containing linearised traits
|]
                    , para [iTrim|
- a `traits.csv` file containing localisation entries
|]
                    , blank
                    , para [iTrim|
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

    let badTraits err = noteFailure [iTrim|
Decoding of traits file ‘${traitsPath}’ failed, aborting:
    ${ err }
|]
    traitsContent <- either badTraits pure =<< fileContents traitsPath

    outputUnavailable <- doesPathExist Hardcoded.outputBase
    when outputUnavailable $ do
        noteFailure [iTrim|
Output destination ‘${Hardcoded.outputBase}’ already exists, aborting.
|]

    when (lineariserHeader `Text.isPrefixOf` traitsContent) $ do
        noteFailure [iTrim|
Traits file ‘${traitsPath}’ seems to already have been auto-generated by this program, aborting. If
you still want to attempt to linearise the file, remove the auto-generation header:

${ Text.unpack lineariserHeader }
|]

    traits <- case runParser traitsStructure traitsPath traitsContent of
        Left errs -> do
            noteFailure [iTrim|
Parsing of traits file failed:

${ errorBundlePretty errs }
|]

        Right traits -> do
            let personalities = length $ _traitsPersonalities traits
                backgrounds   = length $ _traitsBackgrounds traits

            for_ [ ("personality", personalities)
                 , ("background", backgrounds)
                 ] $ \(kind, count) -> when (count == 1) $ do
                      noteFailure [iTrim|
Nothing to linearise as there is only one ${kind::String}, aborting. (Did you run the program on an
already linearised trait file?)
|]

            note' [ [i|Found ${personalities} personalities and ${backgrounds} backgrounds,|]
                  , [i| linearising to ${ personalities * backgrounds } composite traits.|]
                  ]

            pure traits

    let (personalities, backgrounds) = traitsLocalisationKeys traits

    localisation <- fmap (HashMap.unions . concat) . forConcurrently paths $ \base ->
        withConsoleRegion Linear $ \region -> do
            let regionOpener = [i|Adding localisation from base path ‘${base}’…|] :: String
            setConsoleRegion region regionOpener

            let pickCSVs = filter csvExtension
                csvExtension = (== ".csv") . takeExtension

            -- N.b. order is very significant. The key that appears in the first file in lexical[1]
            -- order is the one that sets the translation, subsequent ones are redundant.
            --
            -- [1]: presumably, at any rate, since this has not been tested in depth
            locFiles <- (sort . pickCSVs) <$> listDirectory (localisationPath base)

            finishConsoleRegion region $ regionOpener
                <> [i| found ${ length locFiles } localisation files.|]

            forConcurrently locFiles $ \path -> do
                contents <-
                    localisationContents baseGameLocalisation base $ localisationFile base path
                case runParser localisations path contents of
                    Left errs -> do
                        note [iTrim|
Parsing of one localisation file failed, skipping it:
    ${ errorBundlePretty errs }
|]
                        pure mempty

                    Right keys -> pure keys

    case baseGameLocalisation of
        IncludeBaseGame   -> note [iTrim|
Found ${ HashMap.size localisation } localisation entries, and adding to it ${ HashMap.size baseLocalisation } base game entries.
|]
        NoIncludeBaseGame -> note [i|Found ${ HashMap.size localisation } localisation entries.|]

    let (orphans, entries) =
            lineariseLocalisation localisation personalities backgrounds baseGameLocalisation

    when (not $ null orphans) $ do
        note [iTrim|
The following traits were missing a translation:
    ${ Text.unpack . Text.intercalate ", " $ toList orphans }
(If you did not expect this, you might want to use `--help` and read about `--extra-localisation`
and/or `--include-base-game`.)
|]

    withTempDirectory "." Hardcoded.outputBase $ \outputPath -> do
        let write path = let ?enc = CP1252 in writeFile (outputPath </> path) . Text.unpack
        write "traits.txt" . formatTraits       $ lineariseTraits traits
        write "traits.csv" . formatLocalisation $ entries
        renameDirectory outputPath Hardcoded.outputBase

    note [i|Done!|]
