{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Things that are hardcoded, and might not need to be. Designed to be importad qualified, to highlight
what is being done.

|-}
module Hardcoded
    ( productIdentifier
    , productKey
    , productSeparator
    , productTranslation

    , Importance(..)
    , hpmPalette
    , colour
    , attack
    , defence
    , attackImportance
    , defenceImportance

    , outputBase
    ) where

import qualified Data.Text as Text

import Types

-- | The product of one personality identifier with one background identifier.
productIdentifier :: Identifier -> Identifier -> Identifier
productIdentifier lhs rhs = lhs <> UnquotedIdentifier "x" <> rhs

-- | Same as `productIdentifier`, when working on keys.
productKey :: Key -> Key -> Key
productKey lhs rhs = lhs <> "x" <> rhs

-- | marks the separation between personality mods and background mods
productSeparator :: Text
productSeparator = "####"

-- | The product of one personality translation with one background translation, together with a
-- summary of important stats.
productTranslation :: Text -> Text -> [Text] -> Text
productTranslation personality background stats' =
    personality <> ", " <> Text.toLower background <> statSuffix
  where
    statSuffix = if null stats'
                     then ""
                     else "  " <> stats
    stats = Text.intercalate " " stats'

-- | A scale of importance, for highlighting purposes.
data Importance
    = HighPositive
    | ModeratePositive
    | NoImportance
    | ModerateNegative
    | HighNegative
    deriving stock (Show, Read, Eq, Ord, Enum)

{-

The colour codes that the game UI recognises are set within `interface/core.gfx`, and are even
moddable. For each font that supports colour, there is a `colorcodes` mapping from colour code to
RGB value. These codes can then be used within localisation.

That being said the UI uses its red/green gradient scale to highlight negative & positive amounts
outside of localisation, by relying on the same codes (presumably). For the sake of consistency, we
follow this gradient. This is unfortunate in terms of colour blindness assistance, but localisation
is not the right place to change this: `interface/core.gfx` is (again, presumably).

-}

type Palette = Importance -> ColourCode

-- | Palette using HPM 0.4.6 colours.
data HPMPalette
    = Green -- UI default
    | GreenHouse
    | HPMNeutral
    | BurntUmber
    | Red -- UI default
    deriving stock (Show, Read, Eq, Ord, Enum)

type ColourCode = Text

hpmCodes :: HPMPalette -> ColourCode
hpmCodes = \case
    Green      -> "G"
    GreenHouse -> "h"
    HPMNeutral -> "!"
    BurntUmber -> "u"
    Red        -> "R"

hpmPalette :: Palette
hpmPalette = hpmCodes . toEnum . fromEnum

-- | Insert colour codes at the right place
bracketColourCode :: ColourCode -> Text -> Text
bracketColourCode code item = [i|§${code}${item}§!|]

-- | Coloured localisation.
colour :: Palette -> Text -> Importance -> Text
colour _palette item NoImportance = item
colour  palette item   importance = bracketColourCode (palette importance) item

-- Mods of interest, for highlighting purposes.

attack, defence :: Text
attack  = "attack"
defence = "defence"

attackImportance, defenceImportance :: Decimal -> Importance

attackImportance = \case
    x |  3 <= x            -> HighPositive
    x |  1 <= x && x  <  3 -> ModeratePositive
    x | -3  < x && x <= -1 -> ModerateNegative
    x |       x      <= -3 -> HighNegative
    _ | otherwise          -> NoImportance

defenceImportance = \case
    x |  3 <= x            -> HighPositive
    x |  1 <= x && x  <  3 -> ModeratePositive
    x | -3  < x && x <= -1 -> ModerateNegative
    x |       x      <= -3 -> HighNegative
    _ | otherwise          -> NoImportance

outputBase :: FilePath
outputBase = "out"
