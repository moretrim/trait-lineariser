module Traits
    ( Parser
    , Trait(..), traitName, traitMods
    , Traits(..)
        , traitsNullPersonality, traitsPersonalities, traitsNullBackground, traitsBackgrounds
        , traitsStructure
    , linearise
    , lineariserHeader
    , formatTraits
    ) where

import GHC.Generics                                (Generic)

import Control.Applicative hiding                  (many, some)
import Control.Applicative.Permutations            (runPermutation, toPermutation)
import Control.Monad.Combinators.NonEmpty          (some)
import Control.Lens hiding                         (noneOf)

import Data.Functor
import Data.Functor.Compose
import Data.Functor.Classes
import Data.Foldable
import Data.List.NonEmpty                          (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Void
import Data.Char
import Data.String.Here.Interpolated
import Data.Text                                   (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Megaparsec hiding                      (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

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
    | Comment Text -- ^ Raw comment, be careful when splicing back
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
    sectionKey = lexeme (string' key) <?> [i|entry key ‘${ Text.unpack key }’|]

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

-- | The contents of a `common/traits.txt` file. Parametrised over the container for personalities
-- so as to allow grouping, refer to `linearise`.
data Traits f = Traits
    { _traitsNullPersonality :: Trait -- ^ The `no_personality` entry that the game expects
    , _traitsPersonalities   :: f Trait
    , _traitsNullBackground  :: Trait -- ^ The `no_background` entry that the game expects
    , _traitsBackgrounds     :: NonEmpty Trait
    }
    deriving stock (Generic)
makeLenses ''Traits

deriving instance Show (f Trait) => Show (Traits f)
deriving instance Read (f Trait) => Read (Traits f)
deriving instance Eq   (f Trait) => Eq   (Traits f)
deriving instance Ord  (f Trait) => Ord  (Traits f)

-- | `common/traits.txt` structure, parsed into a `Traits`.
traitsStructure :: Parser (Traits NonEmpty)
traitsStructure = do
    optional ws

    ((nullPersonality, personalities), (nullBackground, backgrounds)) <- runPermutation $

        (,) <$> toPermutation
                (
                    entry "personality" $ do
                        (,) <$> nullTrait "personality" "no_personality" <*> traits "personality"
                )

            <*> toPermutation
                (
                    entry "background" $ do
                        (,) <$> nullTrait "background" "no_background" <*> traits "background"
                )

    eof

    pure Traits
        { _traitsNullPersonality = nullPersonality
        , _traitsPersonalities = personalities
        , _traitsNullBackground = nullBackground
        , _traitsBackgrounds = backgrounds
        }

  where
    nullTrait kind key = (Trait <$> pure key <*> entry key (many' modifier)) <?> descr
      where
        key'  = Text.unpack key
        kind' = Text.unpack kind
        descr =
            [iTrim|mandatory special trait ‘${ key' }’ in first position of entry ‘${ kind' }’|]

    traits kind = some trait <?> [i|at least one ${kind::String} after the special null ${kind}|]

---------------
-- Linearise --
---------------

type Grouped = Compose (Compose NonEmpty ((,) Text)) NonEmpty

-- | Linearise all traits, combining each personality–background combination into a single
-- personality and leaving only a unit background.
linearise :: Traits NonEmpty -> Traits Grouped
linearise traits = traits
    { _traitsPersonalities = Compose . Compose $ do
        personality <- _traitsPersonalities traits
        -- group by personality
        pure . (_traitName personality,) $ do
            traitProduct personality <$> _traitsBackgrounds traits
    , _traitsBackgrounds = pure unitBackground -- not really used by the code, but it’s nice to be
                                               -- thorough
    }
      where
        traitProduct personality background = Trait
            { _traitName = _traitName personality <> "×" <> _traitName background
            , _traitMods = _traitMods personality <> [separator] <> _traitMods background
            }
              where
                -- | marks the separation between personality mods and background mods
                separator = Comment "####"

        unitBackground = Trait
            { _traitName = "unit_background"
            , _traitMods = mempty
            }

------------
-- Format --
------------

indentedLineBreak :: Text -> Int -> Text
indentedLineBreak lineBreak level = lineBreak <> (Text.replicate (level * 4) " ")

offset :: Text -> Int -> [Text] -> Text
offset lineBreak = Text.intercalate . indentedLineBreak lineBreak

offset' :: Int -> [Text] -> Text
offset' level = Text.concat . map (textBreak<>)
  where
    textBreak = indentedLineBreak "\n" level

formatBlock :: Int -> Text -> Text
formatBlock level body = [iTrim|
    {${body}${ if Text.null body then "" else indentedLineBreak "\n" level }}
|]

formatIdentifier :: Text -> Text
formatIdentifier = id

formatMod :: Interspersed (Text, Text) -> Text
formatMod (Interspersed (key, value)) = [iTrim|${key} = ${value}|]
formatMod (Comment comment) = comment

formatTrait :: Int -> Trait -> Text
formatTrait level item = [iTrim|
${ formatIdentifier $ _traitName item } = ${
    formatBlock level . offset' (succ level) . map formatMod $ _traitMods item }
|]

formatGroup :: Int -> (Text, NonEmpty Trait) -> Text
formatGroup level (group, traits) = [iTrim|
## #${group}
${ indentedLineBreak "\n" level }${ offset "\n\n" level . toList $ fmap (formatTrait level) traits }
|]

formatGroups :: Int -> Grouped Trait -> Text
formatGroups level (getCompose . getCompose -> groups) =
    offset' level . toList . fmap (formatGroup level) $ groups

lineariserHeader :: Text
lineariserHeader = [iTrim|
# This linearised trait file has been automatically generated.
# <https://github.com/moretrim/trait-lineariser>
|]

formatTraits :: Traits Grouped -> Text
formatTraits traits = do
    [iTrim|
${lineariserHeader}
#
# This file should be in the WINDOWS-1252 (aka CP-1252) encoding that the game expects. If the
# following phrases do NOT look surrounded by quotation marks, something went wrong & you should
# verify your editor settings:
# - ‘single quotes’
# - “double quotes”
# - «guillemets»
#
# For ease of navigation every group of personality-background pairs is opened by a comment. E.g.
# when looking for the group of all “earnest” pairs, try searching for `#earnest`.

background = {
    ${ formatTrait 1 $ _traitsNullBackground traits }

    ## Looking for all the backgrounds? they have all been merged into the personality-background
    ## pairs just below. Do NOT remove this neutral element background without leaving at least one
    ## background beyond the null background.
    unit_background = {}
}

personality = {
    ${ formatTrait 1 $ _traitsNullPersonality traits }
${ formatGroups 1 $ _traitsPersonalities traits }
}
|]
