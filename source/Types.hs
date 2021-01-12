module Types
    ( Key
    , Keys
    , Identifier(..)
        , identifierProduct
    ) where

import GHC.Generics  (Generic)

import Control.Lens

import Data.Hashable (Hashable)
import Data.HashSet  (HashSet)

import Data.Text     (Text)

data Identifier
    = QuotedIdentifier Text -- ^ Contents only, quotation marks are implied
    | UnquotedIdentifier Text
    deriving stock (Show, Read, Eq, Ord, Generic)
    deriving anyclass (Hashable)
makePrisms ''Identifier

raw :: Identifier -> Text
raw (QuotedIdentifier contents)     = contents
raw (UnquotedIdentifier identifier) = identifier

instance Semigroup Identifier where
    UnquotedIdentifier lhs <> UnquotedIdentifier rhs = UnquotedIdentifier $ lhs     <> rhs
    lhs                    <> QuotedIdentifier rhs   = QuotedIdentifier   $ raw lhs <> rhs
    QuotedIdentifier lhs   <> rhs                    = QuotedIdentifier   $ lhs     <> raw rhs

instance Monoid Identifier where
    mempty = QuotedIdentifier ""

-- | Compute the canonical product of two identifiers.
identifierProduct :: Identifier -> Identifier -> Identifier
identifierProduct lhs rhs = lhs <> UnquotedIdentifier "x" <> rhs

-- | Localisation key.
type Key = Identifier

-- | Set of localisation keys.
type Keys = HashSet Identifier
