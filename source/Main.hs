module Main where

import Prelude hiding                              (getContents, readFile)

import GHC.Generics                                (Generic)

import Control.Applicative hiding                  (many, some)
import Control.Applicative.Permutations
import Control.Lens hiding                         (noneOf)

import Data.Functor
import Data.Void
import Data.Char
import Data.Text                                   (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

-- CLI
import Data.Encoding.UTF8
import Data.Encoding.CP1252
import System.Exit                                 (exitFailure)
import System.IO                                   (stderr, hPutStrLn)
import System.IO.Encoding                          (getContents, readFile)
import Text.Heredoc
import qualified Options.Applicative as Args

type Parser = Parsec Void Text

---------------------------------
-- Victoria 2 specific parsing --
---------------------------------

-- Lexing --

lineComment :: Parser Text
lineComment = Text.cons <$> char '#' <*> takeWhileP (Just "comment character") (/= '\n')

-- | Lax whitespace, i.e. also consumes comments.
ws :: Parser ()
ws = Lex.space space1 (void lineComment) {- no block comments #-} empty

-- | Strict whitespace, does not consume comments.
ws' :: Parser ()
ws' = Lex.space space1 {- no comments of any knid #-} empty empty

-- Convention: strict lexers (that rely on `ws'`) are quoted.

lexeme, lexeme' :: Parser item -> Parser item
lexeme  = Lex.lexeme ws
lexeme' = Lex.lexeme ws'

symbol, symbol' :: Text -> Parser Text
symbol  = Lex.symbol ws
symbol' = Lex.symbol ws'

-- Parsing --

identifier, quotedIdentifier, unquotedIdentifier :: Parser Text

identifier = quotedIdentifier <|> unquotedIdentifier

quotedIdentifier =
    lexeme . between quote quote $ takeWhile1P (Just "quoted identifier character") (/= '"')
      where
        quote = char '"'

unquotedIdentifier = lexeme $ takeWhile1P (Just "identifier character") validIdentifierChar
  where
    validIdentifierChar c = not (isSpace c) && c `notElem` ("\"#={};,()!&" :: String)

-- | For intermingling parse results with original comments.
data Interspersed item
    = Interspersed item
    | Comment Text
    deriving stock (Show, Read, Eq, Ord, Generic)
makePrisms ''Interspersed

-- | Parse an item or a comment.
commented' :: Parser item -> Parser (Interspersed item)
commented' item = try (lexeme' $ Comment <$> lineComment) <|> (Interspersed <$> item)

-- | Parse items interspersed with comments, preserving the comments. NOTE: this functionality is
-- limited.
many' :: Parser item -> Parser [Interspersed item]
many' = many . commented'

-- | `{ body }`
block :: Parser body -> Parser body
block = between (symbol "{") (symbol "}")

-- | `key = value` pair, where key is an identifier and value will be parsed as raw as possible.
identifierPair :: Parser (Text, Text)
identifierPair = do
    key <- identifier
    symbol "="
    value <- takeWhile1P (Just "non-space character") (not . isSpace)
    optional ws
    pure $ (key, value)

-- | `key = { body }`
entry :: Text -> Parser body -> Parser body
entry key body = sectionKey *> symbol "=" *> block body
  where
    sectionKey = lexeme (string' key) <?> ("entry key ‘" <> Text.unpack key <> "’")

------------------------
-- Trait file parsing --
------------------------

-- | A leader trait looks like the following:
--
--     imperious = {
--         attack = 1
--         defence = -2
--         morale = 0.20
--     }
--
-- That is, it is a named collection of army modifiers. In order to be as agnostic as possible,
-- modifiers are stored as key–value pairs of raw text.
data Trait = Trait
    { _traitName :: Text
    , _traitMods :: [Interspersed (Text, Text)]
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
makeLenses ''Trait

modifier :: Parser (Text, Text)
modifier = identifierPair

-- | Trait parser
trait :: Parser Trait
trait = Trait <$> identifier <*> (symbol "=" *> block (many' modifier))

-- | The contents of a `common/traits.txt` file.
data Traits = Traits
    { _traitsNullPersonality :: Trait -- ^ The `no_personality` entry that the game expects
    , _traitsPersonalities   :: [Trait]
    , _traitsNullBackground  :: Trait -- ^ The `no_background` entry that the game expects
    , _traitsBackgrounds     :: [Trait]
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
makeLenses ''Traits

-- | `common/traits.txt` structure, parsed into a `Traits`.
traitsStructure :: Parser Traits
traitsStructure = do
    optional ws

    ((nullPersonality, personalities), (nullBackground, backgrounds)) <- runPermutation $

        (,) <$> toPermutation
                (
                    entry "personality" $ do
                        (,) <$> nullTrait "personality" "no_personality" <*> many trait
                )

            <*> toPermutation
                (
                    entry "background" $ do
                        (,) <$> nullTrait "background" "no_background" <*> many trait
                )

    eof

    pure Traits
        { _traitsNullPersonality = nullPersonality
        , _traitsPersonalities = personalities
        , _traitsNullBackground = nullBackground
        , _traitsBackgrounds = backgrounds
        }

  where
    nullTrait section key = (Trait <$> pure key <*> entry key (many' modifier)) <?> descr
      where
        descr =
            "mandatory special trait ‘"
            <> Text.unpack key
            <> "’ in first position of entry ‘"
            <> Text.unpack section
            <>"’"

---------------
-- Linearise --
---------------

---------
-- CLI --
---------

note :: String -> IO ()
note = hPutStrLn stderr

parseArgs :: IO (Maybe FilePath)
parseArgs = Args.execParser $ Args.info (Args.helper <*> args) desc
  where
    args = optional . Args.argument Args.str $
        Args.help [here|
            Path to the traits file to linearise.
            Reads from standard input if the argument is ‘-’ or is left unspecified.

            Assumes WINDOWS-1252 encoding when reading from a path, and UTF-8 when reading from
            standard input. (In the latter case, iconv(1) can help.)
            |]
        <> Args.metavar "PATH/TO/traits.txt"

    desc =
        Args.fullDesc
        <> Args.progDesc [here|
            Linearise a `<mod-path>/common/traits.txt` mod file. The linearised output is written
            to standard output, while statistics and errors are written to standard error.
            |]

main :: IO ()
main = do
    userSource <- parseArgs
    (source, contents) <- case userSource of
        Just traitsPath | traitsPath /= "-" -> do
            let ?enc = CP1252
            contents <- Text.pack <$> readFile traitsPath
            pure (traitsPath, contents)

        _ -> do
            let ?enc = UTF8
            contents <- Text.pack <$> getContents
            pure ("<stdin>", contents)

    traits <- case runParser traitsStructure source contents of
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
