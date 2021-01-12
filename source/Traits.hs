module Traits
    ( Parser
    , Trait(..), traitName, traitMods
    , Traits(..)
        , traitsNullPersonality, traitsPersonalities, traitsNullBackground, traitsBackgrounds
        , traitsLocalisationKeys
        , traitsStructure
    , lineariseTraits
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
import qualified Data.HashSet as HashSet

import Data.Void
import Data.Char
import Data.String.Here.Interpolated
import Data.Text                                   (Text)
import qualified Data.Text as Text
--import qualified Data.Text.IO as Text

import Text.Megaparsec hiding                      (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Types

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

identifier, quotedIdentifier, unquotedIdentifier :: Parser Identifier

identifier = quotedIdentifier <|> unquotedIdentifier

quotedIdentifier = parser <?> descr
  where
    parser = fmap QuotedIdentifier . lexeme . between quote quote $
        takeWhile1P (Just "quoted identifier character") (/= '"')
    descr = "a quoted identifer (e.g. `\"a quoted identifier\"`)"
    quote = char '"'

unquotedIdentifier = parser <?> descr
  where
    parser = fmap UnquotedIdentifier . lexeme $
        takeWhile1P (Just "identifier character") validIdentifierChar
    descr = "an unquoted identifier (e.g. `unquoted-identifier`)"
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
identifierPair :: Parser (Identifier, Text)
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
    { _traitName :: Identifier
    , _traitMods :: [Interspersed (Identifier, Text)]
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
makeLenses ''Trait

modifier :: Parser (Identifier, Text)
modifier = identifierPair

-- | Trait parser
trait :: Parser Trait
trait = Trait <$> identifier <*> (symbol "=" *> block (many' modifier))

-- | The contents of a `common/traits.txt` file. Parametrised over the container for backgrounds
-- so as to allow grouping, refer to `linearise`.
data Traits f = Traits
    { _traitsNullPersonality :: Trait -- ^ The `no_personality` entry that the game expects
    , _traitsPersonalities   :: NonEmpty Trait
    , _traitsNullBackground  :: Trait -- ^ The `no_background` entry that the game expects
    , _traitsBackgrounds     :: f Trait
    }
    deriving stock (Generic)
makeLenses ''Traits

deriving instance Show (f Trait) => Show (Traits f)
deriving instance Read (f Trait) => Read (Traits f)
deriving instance Eq   (f Trait) => Eq   (Traits f)
deriving instance Ord  (f Trait) => Ord  (Traits f)

-- | Compute localisation keys.
traitsLocalisationKeys :: Traits NonEmpty -> Keys
traitsLocalisationKeys ts = foldMap traitNames [ _traitsPersonalities ts
                                               , _traitsBackgrounds ts
                                               ]
  where
    traitNames = HashSet.fromList . map _traitName . toList

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
    nullTrait kind key =
        (Trait <$> pure (UnquotedIdentifier key) <*> entry key (many' modifier)) <?> descr
      where
        key'  = Text.unpack key
        kind' = Text.unpack kind
        descr =
            [iTrim|mandatory special trait ‘${ key' }’ in first position of entry ‘${ kind' }’|]

    traits kind = some trait <?> [i|at least one ${kind::String} after the special null ${kind}|]

---------------
-- Linearise --
---------------

type Grouped = Compose (Compose NonEmpty ((,) Identifier)) NonEmpty

-- | Linearise all traits, combining each personality–background combination into a single
-- background and leaving only a unit personality.
lineariseTraits :: Traits NonEmpty -> Traits Grouped
lineariseTraits traits = traits
    { _traitsPersonalities = pure unitPersonality -- not really used by the code, but it’s nice to
                                                  -- be thorough
    , _traitsBackgrounds = Compose . Compose $ do
        personality <- _traitsPersonalities traits
        -- group by personality
        pure . (_traitName personality,) $ do
            traitProduct personality <$> _traitsBackgrounds traits
    }
      where
        traitProduct personality background = Trait
            { _traitName = _traitName personality `identifierProduct` _traitName background
            , _traitMods = _traitMods personality <> [separator] <> _traitMods background
            }
              where
                -- | marks the separation between personality mods and background mods
                separator = Comment "####"

        unitPersonality = Trait
            { _traitName = UnquotedIdentifier "unit_personality"
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

formatIdentifier :: Identifier -> Text
formatIdentifier (QuotedIdentifier contents)     = "\"" <> contents <> "\""
formatIdentifier (UnquotedIdentifier identifier) = identifier

formatMod :: Interspersed (Identifier, Text) -> Text
formatMod (Interspersed (key, value)) = [iTrim|${formatIdentifier key} = ${value}|]
formatMod (Comment comment) = comment

formatTrait :: Int -> Trait -> Text
formatTrait level item = [iTrim|
${ formatIdentifier $ _traitName item } = ${
    formatBlock level . offset' (succ level) . map formatMod $ _traitMods item }
|]

formatGroup :: Int -> (Identifier, NonEmpty Trait) -> Text
formatGroup level (group, traits) = [iTrim|
## #${formatIdentifier group}
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
# For ease of navigation every group of personality-background pairs is opened by a comment. E.g.
# when looking for the group of all "earnest" pairs, try searching for `#earnest`.

personality = {
    ${ formatTrait 1 $ _traitsNullPersonality traits }

    ## Looking for all the personalities? they have all been merged into the personality-background
    ## pairs just below. Do NOT remove this neutral element personality without leaving at least
    ## another to replace it. It serves as the stock personality for all leaders. (This is separate
    ## from the `no_personality` entry that the game expects and uses e.g. for leaderless armies and
    ## navies.)
    unit_personality = {}
}

background = {
    ${ formatTrait 1 $ _traitsNullBackground traits }
${ formatGroups 1 $ _traitsBackgrounds traits }
}
|]
