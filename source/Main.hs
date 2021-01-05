module Main where

import GHC.Generics                     (Generic)

import Control.Applicative hiding       (many, some)
import Control.Applicative.Permutations
import Control.Lens

import Data.Text                        (Text)
import qualified Data.Text as Text
import Text.Megaparsec

-- | A leader trait is a collection of army modifiers, each usually of the form:
--
--     some_statistic_or_other = 3.0
--
-- In order to be as agnostic as possible, modifiers are stored as key–value pairs of raw text.
data Trait = Trait
    { _traitName :: Text
    , _traitMods :: [(Text, Text)]
    }
    deriving stock (Show, Read, Eq, Ord, Generic)
makeLenses ''Trait

-- | `common/traits.txt` structure, parsed into a list of personalities and backgrounds.
traitFile = undefined

main :: IO ()
main = print $ Trait "aggressive" [("attack", "1")]
