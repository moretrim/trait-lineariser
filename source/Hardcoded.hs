{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Things that are hardcoded, and might not need to be. Designed to be imported qualified, to highlight
what is being done.

|-}
module Hardcoded
    ( traitsOutput, localisationOutput, oobOutput
    , unitPersonalityIdentifier, productIdentifier
    , productKey
    , productSeparator
    , productTranslation

    , Importance(..)
    , hpmPalette
    , colour

    , importanceSymbol
    , integralBlanks, percentageBlanks
    , symbolBlanks
    , formatDecimal, formatDecimal', formatInteger, formatPercentage

    , attack
    , defence
    , speed
    , organisation
    , morale

    , attackImportance
    , defenceImportance
    , speedImportance
    , extrasImportance

    , outputBase
    ) where


import qualified Data.Text as Text
import Text.Printf                 (printf)

import System.FilePath             ((</>))

import Types

-- | Path to linearised trait file.
traitsOutput :: FilePath
traitsOutput = "common" </> "traits.txt"

-- | Path to linearised trait localisation.
localisationOutput :: FilePath
localisationOutput = "localisation" </> "linearised-traits.csv"

-- | Path to linearised oob directory.
oobOutput :: FilePath -> FilePath
oobOutput path = "history" </> "units" </> path

-- | Identifier of the unit personality.
unitPersonality :: Text
unitPersonality = "unit_personality"

unitPersonalityIdentifier :: Identifier
unitPersonalityIdentifier = UnquotedIdentifier unitPersonality

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
-- summary of important stats. Anatomy of what an impossibly good leader might look like (starting
-- after the indent):
--
--     +2 +2 40% !!! Personality, background
--     ^^ ^^ ^^^ ^^^
--     |  |  |   |
--     |  |  |   +—– other stats of interest
--     |  |  +—————– speed modifier
--     |  +————————– signed defence
--     +———————————– signed attack
--
-- Care is taken that the leading stat summary has good alignment, so that it looks like a column in
-- lists of leaders.
productTranslation :: Text
                   -> Text
                   -> (Text, Text, Text, Text)
                   -> Text
productTranslation personality background (attack, defence, speed, extras) =
    stats <> " " <> traits
  where
    stats = attack <> " " <> defence <> " " <> speed <> extras
    traits = personality <> ", " <> Text.toLower background

-- | A scale of importance, for highlighting purposes.
data Importance
    = HighPositive
    | ModeratePositive
    | NoImportance
    | ModerateNegative
    | HighNegative
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

-- Saturated arithmetic
instance Semigroup Importance where
    lhs <> rhs = saturate $ magnitude lhs + magnitude rhs
      where
        centre = fromEnum NoImportance
        magnitude importance = centre - fromEnum importance
        saturate dist = case centre - dist of
            x | x < (fromEnum @Importance minBound) -> minBound
            x | x > (fromEnum @Importance maxBound) -> maxBound
            x | otherwise                           -> toEnum x

instance Monoid Importance where
    mempty = NoImportance

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
colour :: Palette -> Importance -> Text -> Text
colour _palette NoImportance item = item
colour  palette   importance item = bracketColourCode (palette importance) item

{-

Because Victoria II uses proportional fonts, it’s not really possible to align stat summaries
cleanly right out of the box.

To be specific the following has been calibrated for the fonts used in:

- the traits in the entries of the leader list in the country military panel (top left)
- the traits in the entries of the leader list in the leader assignment panel (from the unit panel)

All consistently make use of Arial 12pt. (According to Wikipedia this should also work for Arimo,
Helvetica, and Liberation Sans typefaces.)

There are other instances (e.g. tooltips) where other fonts are used, meaning that alignment won’t
work there. Thankfully it shouldn’t matter too much, because we only want the alignment for *lists*
of leader traits!

Note that it is relatively easy (though somewhat time consuming) to pick appropriate highlight
symbols and/or substitutions. Victoria II ships with bitmap fonts in the form of a .fnt file
together with a .tga texture. The font file contains the glyph data we’re interested in,
specifically in the form of the `xadvance` column which describes the horizontal space that a glyph
takes. As long as you are mindful of the kernings which subtract from this base width, it’s possible
to count e.g. how many non-breaking space characters can substitute a sequence of other characters.

Some example Awk invocations that proved useful, here grouping & displaying all characters according to
their `xadvance` value for Arial 12pt:

```shell-session
$ awk '/xadvance=/ { print substr($9, length("xadvance=_")) }' gfx/fonts/Arial12.fnt | sort -h | uniq
>2
>3
>4
>5
>6
>7
>8
>9
>10
>11

$ for advance in {2..11}; do
    echo "\n=== characters with xadvance=$advance ==="
    LC_ALL=WINDOWS-1252 awk -f - gfx/fonts/Arial12.fnt <<EOF | iconv --from-code=WINDOWS-1252 | fmt | tail -c+2
    BEGIN { printf "." }
    /xadvance=$advance\s/ { printf "%c ", +substr(\$2, 4) }
    END { print "" }
EOF
done
>
>=== characters with xadvance=2 ===
>! ' I i j l | ‚ ‘ ’ ¡ ¦ Ì Í Î Ï ì í î ï
>
>=== characters with xadvance=3 ===
>  , . / : ; [ \ ] t  
>
>=== characters with xadvance=4 ===
>" ( ) * - ` f r { } „ ˆ ‹ “ ” • ˜ › ¨ ª ­ ° ² ³ ´
>· ¸ ¹
>
>=== characters with xadvance=5 ===
>J ^ k º
>
>=== characters with xadvance=6 ===
># $ + 0 1 2 3 4 5 6 7 8 9 < = > ? E F L P T _ a b c d e g h n o p q s
>u v x y z ~ € ƒ † ‡ – š ž ¢ £ ¥ § « ¬ ¯ ± µ ¶ »
>¿ È É Ê Ë × à á â ã ä å ç è é ê ë ð ñ ò ó ô õ ö
>÷ ø ù ú û ü ý þ ÿ
>
>=== characters with xadvance=7 ===
>& B C D H K N R S U X Z Š Ž ¤ Ç Ñ Ù Ú Û Ü Þ ß
>
>=== characters with xadvance=8 ===
>A G M O Q V Y m Ÿ © ® À Á Â Ã Ä Å Ð Ò Ó Ô Õ Ö Ø Ý
>
>=== characters with xadvance=9 ===
>¼
>
>=== characters with xadvance=10 ===
>% W w œ ½ ¾ æ
>
>=== characters with xadvance=11 ===
>@ … ‰ Œ — ™ Æ
```

(Using ‘>’ to indicate the left margin of the output: mind that 0x20 SPACE makes an appearance at
the start of a line! 0xA0 NO-BREAK SPACE also makes a more mundane appearance later.)

A particular standout here is 0x25 PERCENT SIGN with an enormous advance of 10. HPM in fact tweaks
its own `Arial12.fnt` to put it at 9.

Tweaked font
------------

To make thinks easier for us, we ship a further tweaked `Arial12_numeric` font modifying a couple
glyphs to make things more regular:

- a thinner percent sign glyph with advance of 6
- a shorter en dash glyph with advance of 6 and a height that puts it at the same level as PLUS
  SIGN—essentially repurposing it as a minus sign while leaving HYPHEN-MINUS unchanged as it is used
  in translations as a regular hyphen

All alignment & width computations will be performed with reference to this custom font which
replaces the stock Arial 12pt.

-}

-- | Convey importance through a symbol (width of 6).
importanceSymbol :: Importance -> Text
importanceSymbol = \case
    HighPositive     -> "!!!"
    ModeratePositive -> "?"
    NoImportance     -> symbolBlanks
    ModerateNegative -> "¿"
    HighNegative     -> "¡¡¡"

integralBlanks, percentageBlanks, symbolBlanks :: Text

-- | Blank substitution for e.g. “+2” or similar, i.e. width of 2×6
integralBlanks = "    "
-- | Blank substitution for e.g. “+40%” or similar, i.e. width of 3×6 + 10 + 2
percentageBlanks = "        "
-- | Blank substitution for “?” or similar, i.e. width of 1×6
symbolBlanks  = "  "

-- | Numeric formatting fixes:
-- - replace ‘-’ HYPHEN-MINUS by ‘–’ EN DASH due to differences in widths relative to ‘+’ PLUS SIGN,
--   and also refer to the notes about the tweaked font
-- - replace ‘ ’ SPACE (introduced by padding) by ‘  ’ 2×NO-BREAK SPACE, which has the correct width
--   & semantics
numericFixes :: Text -> Text
numericFixes = spaces . hyphens
  where
    hyphens = Text.replace "-" "–"
    spaces  = Text.replace " " "  "

-- Really belong to `Format` (which in fact re-exports them), but they are used here and we want
-- this module early in the dependency hierarchy.
formatDecimal, formatDecimal' :: Decimal -> Text

-- | Displays a decimal as it was stored.
formatDecimal = Text.pack . show @Decimal

-- | Like `formatDecimal`, but with a plus sign for non-negative quantities.
formatDecimal' quantity = plusSign $ formatDecimal quantity
  where
    plusSign = if quantity >= 0
                   then ("+" <>)
                   else id

formatInteger, formatPercentage :: Decimal -> Text

-- | Format small integer with sign, e.g. “-2”.
formatInteger = numericFixes . formatDecimal'

-- | Format modifier as a percent change, e.g. “+05%”. Note the fixed number of columns.
formatPercentage quantity = numericFixes . Text.pack $ printf "%+3d%%" percentage
  where
    percentage = round @Decimal @Int $ quantity * 100

-- Mods of interest, for highlighting purposes.

attack, defence, speed, organisation, morale :: Text
attack       = "attack"
defence      = "defence"
speed        = "speed"
organisation = "organisation"
morale       = "morale"

attackImportance, defenceImportance, speedImportance :: Decimal -> Importance

attackImportance = \case
    x |  3 <= x            -> HighPositive
    x |  1 <= x && x <   3 -> ModeratePositive
    x | -3 <  x && x <= -1 -> ModerateNegative
    x |            x <= -3 -> HighNegative
    _ | otherwise          -> NoImportance

defenceImportance = \case
    x |  3 <= x            -> HighPositive
    x |  1 <= x && x <   3 -> ModeratePositive
    x | -3 <  x && x <= -1 -> ModerateNegative
    x |            x <= -3 -> HighNegative
    _ | otherwise          -> NoImportance

speedImportance = \case
    x |  0.25 <  x               -> HighPositive
    x |  0.00 <  x && x <=  0.25 -> ModeratePositive
    x | -0.25 <= x && x <   0.00 -> ModerateNegative
    x |               x <  -0.25 -> ModerateNegative
    _ | otherwise                -> NoImportance

nothingScale :: Maybe Decimal -> Decimal
nothingScale = fromMaybe 0

extrasImportance :: Maybe Decimal -> Maybe Decimal -> Importance
extrasImportance (nothingScale -> organisation) (nothingScale -> morale) =
    organisationImportance <> moraleImportance
  where
    organisationImportance = case organisation of
        x |  0.15 <  x               -> HighPositive
        x |  0.05 <  x && x <=  0.15 -> ModeratePositive
        x | -0.15 <  x && x <  -0.05 -> ModerateNegative
        x |               x <= -0.15 -> HighNegative
        _ | otherwise                -> NoImportance

    moraleImportance = case morale of
        x |  0.15 <  x               -> HighPositive
        x |  0.05 <  x && x <=  0.15 -> ModeratePositive
        x | -0.15 <  x && x <  -0.05 -> ModerateNegative
        x |               x <= -0.15 -> HighNegative
        _ | otherwise                -> NoImportance

outputBase :: FilePath
outputBase = "output"
