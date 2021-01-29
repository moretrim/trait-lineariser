{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Common types.

|-}
module Types
    ( BaseGameLocalisation(..)

    , Interspersed(..)
        , _Parsed, _Comment, commentOut, commentOut'
        , apI, liftI2

    , Identifier(..)
        , unquote
    , Key, OrderedKeys
    , Translation, Translations, each', each''

    , Entry
    , Localisation, OrderedLocalisation

    -- Convenience re-exports

    , module GHC.Generics

    , module Control.Applicative
    , module Control.Monad
    , module Control.Lens

    , module Data.Functor
    , module Data.Traversable
    , module Data.Foldable
    , module Data.Void
    , module Data.Maybe
    , module Data.List
    , module Data.List.NonEmpty
    , module Data.Map.Lens
    , module Data.HashMap.Strict

    , module Data.Char
    , module Data.String.Here.Interpolated
    , module Data.Text

    , module Data.Ratio
    , module Data.Decimal
    ) where

import GHC.Generics        (Generic)
import Unsafe.Coerce

import Control.Applicative
import Control.Monad
import Control.Lens hiding (noneOf)

import Data.Functor
import Data.Traversable
import Data.Foldable
import Data.Void
import Data.Maybe
import Data.List hiding    (uncons)
import Data.List.NonEmpty  (NonEmpty(..))
import Data.Map.Lens
import Data.HashMap.Strict (HashMap)

import Data.Char
import Data.String.Here.Interpolated
import Data.Text           (Text)
import qualified Data.Text as Text
import Data.Ratio          ((%))
import Data.Decimal        (Decimal)

---------------------
-- Program options --
---------------------

data BaseGameLocalisation
    = IncludeBaseGame
    | NoIncludeBaseGame
    deriving stock (Show, Read, Eq, Ord, Enum, Generic)

---------------------
-- General parsing --
---------------------

-- | For intermingling parse results with original comments.
data Interspersed item
    = Parsed item
    | Comment Text -- ^ Raw comment, be careful when splicing back
    deriving stock (Show, Read, Eq, Ord, Generic, Functor, Foldable, Traversable)
makePrisms ''Interspersed

-- | Turn an `Interspersed` data constructor into a `Comment` (as specified by the `Show` instance),
-- if it isn’t one already.
commentOut :: (Show item) => Interspersed item -> Interspersed item'
commentOut (Parsed item)   = Comment . Text.pack $ show item
commentOut original@(Comment {}) = unsafeCoerce original

-- | Project an `Interspersed` item to either its textual representation if present, or the verbatim
-- comment it is otherwise.
commentOut' :: (Show item) => Interspersed item -> Text
commentOut' (Parsed item) = Text.pack $ show item
commentOut' (Comment comment)   = comment

-- | <*> for Interspersed.
apI :: (Show a, Show b)
    => Interspersed (a -> b) -> Interspersed a -> Interspersed b
apI (Parsed f)  (Parsed a) = Parsed $ f a
apI (Parsed {}) rhs        = commentOut rhs
apI (Comment f)       rhs  = Comment $ f <> " " <> commentOut' rhs

-- | liftA2 for Interspersed.
liftI2 :: (Show a, Show b, Show c)
       => (a -> b -> c) -> Interspersed a -> Interspersed b -> Interspersed c
liftI2 f lhs rhs = fmap f lhs `apI` rhs

------------
-- Script --
------------

-- | Script-side identifier. Requires careful use, as e.g. `QuotedIdentifier
-- "I-dont-require-quotes-in-script"` and `UnquotedIdentifier "I-dont-require-quotes-in-script"`
-- point to the same referent in-engine (e.g. for the purpose of localisation).
--
-- For this reason we do NOT derive `Hashable`, to try & catch attempts at using `Identifier` as
-- `HashMap` keys. Also see `unquote`.
data Identifier
    = QuotedIdentifier Text -- ^ Contents only, quotation marks are implied
    | UnquotedIdentifier Text
    deriving stock (Show, Read, Eq, Ord, Generic)
makePrisms ''Identifier

-- | May require careful use, as it can be unhygienic: unquoted identifiers may not be valid script.
unquote :: Identifier -> Text
unquote (QuotedIdentifier contents)     = contents
unquote (UnquotedIdentifier identifier) = identifier

instance Semigroup Identifier where
    UnquotedIdentifier lhs <> UnquotedIdentifier rhs = UnquotedIdentifier $ lhs         <> rhs
    lhs                    <> QuotedIdentifier rhs   = QuotedIdentifier   $ unquote lhs <> rhs
    QuotedIdentifier lhs   <> rhs                    = QuotedIdentifier   $ lhs         <> unquote rhs

instance Monoid Identifier where
    mempty = QuotedIdentifier ""

------------------
-- Localisation --
------------------

{-
A point of terminology. The typical CSV localisation file looks like the following:

    ### This localisation file has been automatically generated ###;English;French;German;Polish;Spanish;Italian;Swedish;Czech;Hungarian;Dutch;Portuguese;Russian;Finnish;
    MILITARY_PERSONALITY;Personality: §Y$NAME$§W;Personnalité : §Y$NAME$§W;Persönlichkeit: §Y$NAME$§W;;Personalidad: §Y$NAME$§W;;;;;;;;;

(Simplified to a CSV header followed by another line.)

First, some game quirks:

- these “CSV” files really involve semicolon separators (which is not too unusual, to be fair)
- using number signs ‘#’ to introduce comments of sorts is a convention—this is expanded upon in the
  following
- the header of sorts on the first line is also a convention
- very little quoting is supported by the game, all quotation marks are taken verbatim
- instead there is a little bit of escaping, e.g. a literal “\n” will be interpreted as a newline
- these are not technically text files because in this pre-Unicode era of PDS games they liked to
  mix encodings, which can be a true headache—see `Localisation` for more details
- there are more interesting behaviours, but they are outside the scope of this program

The anatomy of a line is as follows:

    KEY;TRANSLATION;TRANSLATION;TRANSLATION;…

- we call a single a line a row as well as a (localisation) entry
- the first row item is the (localisation) key: it corresponds to an in-game or in-script item which
  can be subject to quoting there, but never in the localisation as per the quirks
- consequently pseudo-comment keys of the kind “## meant to be a comment;;;…” are actually reachable
  from script, but it has to be deliberate so it’s fine
- each item subsequent to the key is one of its translations (one of the purposes of our nice
  headers is to remind the reader which translation is which)
- anything beyond the last item is crud, of which the game is fairly tolerant (see `Localisation`)

-}

-- | Localisation key. Not to be confused with script-side identifiers, They are always unquoted and
-- raw.
type Key = Text

-- | Ordered set of localisation keys. That means it can be assumed there are no duplicates.
type OrderedKeys = NonEmpty Key

-- | Not everything is translated into every language, especially with mods.
type Translation = Maybe Text

-- | The translations expected of this era of PDS games. We have to be lenient because malformed
-- localisation files are the norm rather than the exception, and the game itself accepts them.
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

-- | Localisation data: keys that map to translations.
type Localisation = HashMap Text Translations

-- | Ordered localisation data.
type OrderedLocalisation = [Entry]
