module Main where

import Prelude hiding                              (getContents, readFile)

import Control.Applicative

import Data.Encoding.UTF8
import Data.Encoding.CP1252

import Data.Maybe
import Data.Text                             (Text)
import qualified Data.Text as Text

import Text.Heredoc
import Text.Megaparsec

import System.Exit                           (exitFailure)
import System.FilePath                       ((</>))
import System.IO                             (stderr, hPutStrLn)
import System.IO.Encoding                    (getContents, readFile)

import qualified Options.Applicative as Args

import Traits

note :: String -> IO ()
note = hPutStrLn stderr

parseArgs :: IO (Maybe FilePath)
parseArgs = Args.execParser $ Args.info (Args.helper <*> args) desc
  where
    args = optional . Args.argument Args.str $
        Args.help [here|
            Path to the mod base path. From this path, the traits file is expected to be located at
            `common/traits.txt`. (If the mod is not yet providing a traits file, you should copy
            over the unmodded traits file.)

            Additionally, localisation files are expected to be located in the `location`
            subdirectory. Localisation keys for each of the trait are required to be present there
            in order for the linearised localisation to be produced. If this is not already the
            case, consider temporarily copying unmodded localisation files.

            The current working directory is assumed if the path is not provided. All files will be
            assumed to be using the WINDOWS-1252 encoding that the game expects. |]
        <> Args.metavar "PATH/TO/MOD"

    desc =
        Args.fullDesc
        <> Args.progDesc [here|
            Linearise the personalities and backgrounds that generals and admirals can have. Output
            consists of a `traits.txt` file containing linearised traits, and a `traits.csv` file
            containing localisation keys. Both files will be output to the current working
            directory.
            |]

main :: IO ()
main = do
    modPath <- fromMaybe "." <$> parseArgs
    let traitsPath       = modPath </> "common" </> "traits.txt"
        localisationPath = modPath </> "localisation"

    let ?enc = CP1252
    traitsContent <- Text.pack <$> readFile traitsPath

    traits <- case runParser traitsStructure traitsPath traitsContent of
        Left errs -> do
            note $ "Parsing of traits file failed:\n\n" <> (errorBundlePretty errs)
            exitFailure

        Right traits -> do
            let personalities = length $ _traitsPersonalities traits
                backgrounds   = length $ _traitsBackgrounds traits
            note $
                "Found "
                <> show personalities
                <> " personalities and "
                <> show backgrounds
                <> " backgrounds, linearising to "
                <> show (personalities * backgrounds)
                <> " composite traits."

            pure traits

    print traits
