{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Common types.

|-}
module Types
    ( BaseGameLocalisation(..)
    , Key, OrderedKeys
    , Translation, Translations, each', each''
    , Entry
    , Localisation, OrderedLocalisation
    , Identifier(..)
        , unquote
        , identifierProduct
    ) where

import GHC.Generics        (Generic)

import Control.Lens

import Data.List.NonEmpty  (NonEmpty)
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap)

import Data.Text           (Text)

data BaseGameLocalisation
    = IncludeBaseGame
    | NoIncludeBaseGame
    deriving stock (Show, Read, Eq, Ord, Generic)

data Identifier
    = QuotedIdentifier Text -- ^ Contents only, quotation marks are implied
    | UnquotedIdentifier Text
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable)
makePrisms ''Identifier

-- | Requires careful use, as it is unhygienic! Unquoted identifiers may not be valid script.
unquote :: Identifier -> Text
unquote (QuotedIdentifier contents)     = contents
unquote (UnquotedIdentifier identifier) = identifier

instance Semigroup Identifier where
    UnquotedIdentifier lhs <> UnquotedIdentifier rhs = UnquotedIdentifier $ lhs         <> rhs
    lhs                    <> QuotedIdentifier rhs   = QuotedIdentifier   $ unquote lhs <> rhs
    QuotedIdentifier lhs   <> rhs                    = QuotedIdentifier   $ lhs         <> unquote rhs

instance Monoid Identifier where
    mempty = QuotedIdentifier ""

-- | Compute the canonical product of two identifiers.
identifierProduct :: Identifier -> Identifier -> Identifier
identifierProduct lhs rhs = lhs <> UnquotedIdentifier "x" <> rhs

-- | Localisation key. Not to be confused with script-side identifiers, They are always unquoted and
-- raw.
type Key = Text

-- | Ordered set of localisation keys.
type OrderedKeys = NonEmpty Key

-- | Not everything is translated into every language, especially with mods.
type Translation = Maybe Text

-- | The translation entries expected of this era of PDS games. We have to be lenient because
-- malformed localisation files are the norm rather than the exception, and the game itself accepts
-- them.
type Translations =
    ( Translation -- ^ English
    , Translation -- ^ French
    , Translation -- ^ German
    , Translation -- ^ Polish
    , Translation -- ^ Spanish
    , Translation -- ^ Italian
    , Translation -- ^ Swedish
    , Translation -- ^ Czech
    , Translation -- ^ Hungarian
    , Translation -- ^ Dutch
    , Translation -- ^ Portuguese
    , Translation -- ^ Russian
    , Translation -- ^ Finnish
    )

-- | Control.Lens.Tuple.each only goes up to 10, which is just our luck. In any case there’s only so
-- much one can do with tuples, too.
each' :: Translations -> [Translation]
each' ( english
      , french
      , german
      , polish
      , spanish
      , italian
      , swedish
      , czech
      , hungarian
      , dutch
      , portuguese
      , russian
      , finnish
      ) = [ english
          , french
          , german
          , polish
          , spanish
          , italian
          , swedish
          , czech
          , hungarian
          , dutch
          , portuguese
          , russian
          , finnish
          ]

-- | See each'.
each'' :: [Translation] -> Translations
each'' [ english
       , french
       , german
       , polish
       , spanish
       , italian
       , swedish
       , czech
       , hungarian
       , dutch
       , portuguese
       , russian
       , finnish
       ] = ( english
           , french
           , german
           , polish
           , spanish
           , italian
           , swedish
           , czech
           , hungarian
           , dutch
           , portuguese
           , russian
           , finnish
           )
each'' _ =
    error "Types.each'': non-exhaustive pattern match, is the argument iso to `Translations`?"

-- | Localisation entry: a key with its associated translations.
type Entry = (Key, Translations)

-- | Localisation data: keys that map to translations. Note that the keys are stored unquoted.
type Localisation = HashMap Text Translations

-- | Ordered localisation data.
type OrderedLocalisation = [Entry]
