{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Things that are hardcoded, and might not need to be. Designed to be importad qualified, to highlight
what is being done.

|-}
module Hardcoded where

import qualified Data.Text as Text

import Types

-- | The product of one personality identifier with one background identifier.
productIdentifier :: Identifier -> Identifier -> Identifier
productIdentifier lhs rhs = lhs <> UnquotedIdentifier "x" <> rhs

-- | Same as `productIdentifier`, when working on keys.
productKey :: Key -> Key -> Key
productKey lhs rhs = lhs <> "x" <> rhs

-- | How the body of a product trait should be organised before displaying.
productTraitTemplate :: [Interspersed item] -> [Interspersed item] -> [Interspersed item]
productTraitTemplate personality background = personality <> [separator] <> background
  where
    -- | marks the separation between personality mods and background mods
    separator = Comment "####"

-- | The product of one personality translation with one background translation.
productTranslation :: Text -> Text -> Text
productTranslation personality background = personality <> ", " <> Text.toLower background

outputBase :: FilePath
outputBase = "out"
