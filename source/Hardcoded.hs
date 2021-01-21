{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Things that are hardcoded, and might not need to be. Designed to be importad qualified, to highlight
what is being done.

|-}
module Hardcoded where

import Data.Text                                   (Text)
import qualified Data.Text as Text

-- | The product of one personality localisation with one background localisation.
productLocalisation :: Text -> Text -> Text
productLocalisation personality background = personality <> ", " <> Text.toLower background

outputBase :: FilePath
outputBase = "out"
