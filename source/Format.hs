{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Text formatting helpers.

|-}
module Format
    ( indent, indentedLineBreak
    , offsetWith, offset
    , formatBlock, formatLines

    , Hardcoded.formatDecimal, Hardcoded.formatDecimal'
    ) where

import qualified Data.Text as Text

import qualified Hardcoded
import Types

indent :: Int -> Text
indent level = Text.replicate (level * 4) " "

indentedLineBreak :: Text -> Int -> Text
indentedLineBreak lineBreak level = lineBreak <> indent level

-- | Set a block of lines off the margin, using a custom linebreak. This includes the first line
-- (use `Text.intercalate` otherwise).
offsetWith :: Text -> Int -> [Text] -> Text
offsetWith lineBreak level = Text.concat . fmap (indentedLineBreak lineBreak level <>)

offset :: Int -> [Text] -> Text
offset = offsetWith "\n"

-- | Format a braced block from verbatim contents.
formatBlock :: Int -> Text -> Text
formatBlock level body = [iTrim|
{${body}${ if Text.null body then "" else indentedLineBreak "\n" level }}
|]

-- | Format a braced block from lines.
formatLines :: Int -> [Text] -> Text
formatLines level = formatBlock level . offset (succ level)
