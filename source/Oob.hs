{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Unit history parsing & linearising. Oob stands for “order of battle”, and this module deals in fact
in oob files.

|-}
module Oob
    ( oob
    , lineariseOob
    , formatOob
    ) where

import Control.Monad.Writer.Strict

import qualified Data.Text as Text

import qualified Hardcoded
import Types.Parsing
import Format

leaderKind' :: MonadParsec errors Text parser
            => parser LeaderKind
leaderKind' =
        LeaderGeneral <$ keyword' "land"
    <|> LeaderAdmiral <$ keyword' "sea"

leader :: MonadParsec errors Text parser
       => parser Leader
leader = section "leader" . runPermutation $
        Leader
    <$> leaderEntry        "name" identifier'
    <*> leaderEntry'    "picture"     scalar'
    <*> leaderEntry        "date"       date'
    <*> leaderEntry        "type" leaderKind'
    <*> leaderEntry "personality" identifier'
    <*> leaderEntry  "background" identifier'
    <*> leaderEntry'   "prestige"    decimal'

  where
    leaderEntry  key value = toPermutation                            $  entryWithTrail key value
    leaderEntry' key value = toPermutationWithDefault Nothing $ Just <$> entryWithTrail key value

    entryWithTrail key value = do
        parsed <- entry key value
        trail  <- try (combine <$> raw ws' <*> lineComment <* ws) <|> (mempty <$ ws)
        pure (parsed, trail)

    -- restore the space character that was consumed by the just-preceding strict parser
    combine whitespace trail = " " <> whitespace <> trail

unit :: MonadParsec errors Text parser
     => parser Unit
unit = uncurry Unit <$> pair kind (block $ many entries)
  where
    kind =
            UnitArmy <$ keyword "army"
        <|> UnitNavy <$ keyword "navy"

    entries =
               LeaderEntry <$> leader
        <|> LeaderFragment <$> blob

-- | Order of battle file. Focusses on leader entries only (either top-level or as part of a unit
-- definition), the rest is presented as raw fragments (including leaderless unit definitions).
--
-- Computes `Right structure` if a leader definition was found, `Left raw` otherwise.
oob :: Parser (Either Text Oob)
oob = finish $ do

    opener    <- optional $ OobFragment <$> raw ws
    fragments <- many fragment
    eof

    pure $ maybe fragments (:fragments) opener

  where
    finish parser = do
        (verbatim, (parsed, getAny -> anyLeader)) <- match $ runWriterT parser
        if anyLeader
            then pure $ Right parsed
            else pure $ Left verbatim

    found p = p <* tell (Any True)

    fragment = aLeader <|> aUnit <|> aBlob

    aLeader = found $   OobLeader <$> leader
    aBlob   =         OobFragment <$> blob

    aUnit = do
        (verbatim, parsed) <- match unit
        let hasLeader = has (unitEntries . traverse . _LeaderEntry)
        if hasLeader parsed
            then found . pure $     OobUnit parsed
            else         pure $ OobFragment verbatim

-- | Linearise personality & background traits inside leader definitions.
lineariseOob :: [OobEntry] -> [OobEntry]
lineariseOob = fmap lineariseOobEntry
  where
    lineariseOobEntry (OobLeader leader) = OobLeader $ lineariseLeader leader
    lineariseOobEntry     (OobUnit unit) =   OobUnit $   lineariseUnit unit
    lineariseOobEntry           verbatim =  verbatim

    lineariseUnit unit = unit { _unitEntries = lineariseEntry <$> _unitEntries unit }

    lineariseEntry (LeaderEntry leader) = LeaderEntry $ lineariseLeader leader
    lineariseEntry             verbatim =    verbatim

    lineariseLeader leader = leader
        { _leaderPersonality = (Hardcoded.unitPersonalityIdentifier, personalityTrail)
        , _leaderBackground = (personality `Hardcoded.productIdentifier` background
                              , backgroundTrail
                              )
        }
          where
            (personality, personalityTrail) = _leaderPersonality leader
            (background, backgroundTrail)   = _leaderBackground  leader

------------
-- Format --
------------

formatTrailCommented :: (item -> Text) -> TrailCommented item -> Text
formatTrailCommented formatItem (item, trail) = formatItem item <> trail

{-|

Unlike e.g. `Traits`, the formatting here is a little less focused on lines. The primary reason
for this is the fact that the output is not entirely new text, but must instead deal with and
include verbatim fragments. Consider for instance that these original fragments may end in a
trailing sequence of whitespace.

Consequently instead of handling lines separated by varying amount of sometimes indented linebreaks,
the formatting here deals in textual blobs, each coming with an separator to “link up” with the next
fragment in a chain.

In the following we distinguish between those fragments of “fresh” content, which we are free to
format however we want; and fragments of original blobs of text which come in mostly verbatim.

-}
type Fragment = (Text, Text)

joinFragments :: Foldable cont => cont Fragment -> Text
joinFragments = fst . foldl' joiner ("", "")
  where
    joiner (soFar, sep) (fragment, nextSep) = (soFar <> sep <> fragment, nextSep)

joinMap :: (Functor cont, Foldable cont) => (a -> Fragment) -> cont a -> Text
joinMap = (joinFragments .) . fmap

-- | Intended usage:
--
--     myFormattedSection = formatFreshContents [iTrim|
--     section-key = ${
--         sectionValueAntiquotation
--     }
--     |]
--
-- This ends the section by a linebreak, which cannot otherwise be specified through the `iTrim`
-- quasiquoter.
formatFreshContents :: Text -> Text
formatFreshContents = (`Text.snoc` '\n')

formatLeaderKind :: LeaderKind -> Text
formatLeaderKind = \case LeaderGeneral -> "land"; LeaderAdmiral -> "sea"

formatLeader :: Int -> Leader -> Text
formatLeader level leader = formatFreshContents [iTrim|
leader = ${
    formatLines level $ catMaybes [
        entry  "name"        formatIdentifier _leaderName,
        entry' "picture"     id               _leaderPicture,
        entry  "date"        id               _leaderDate,
        entry  "type"        formatLeaderKind _leaderKind,
        entry  "personality" formatIdentifier _leaderPersonality,
        entry  "background"  formatIdentifier _leaderBackground,
        entry' "prestige"    formatDecimal    _leaderPrestige
    ]
}
|]
  where
    formatPair name value = [i|${name::Text} = ${value}|]
    formatAttr name formatValue attr = formatTrailCommented (formatPair name . formatValue) attr
    entry :: Text -> (item -> Text) -> (Leader -> TrailCommented item) -> Maybe Text
    entry  name formatValue focus = pure . formatAttr name formatValue  $  focus leader
    entry' :: Text -> (item -> Text) -> (Leader -> Maybe (TrailCommented item)) -> Maybe Text
    entry' name formatValue focus =        formatAttr name formatValue <$> focus leader

-- | The fragment link for fresh contents. Visually, in the intended (pseudo-)output:
--
--     fresh-section = {
--         fresh-contents
--     }<fresh section linebreak>
--     <<fresh separator>>
--     fresh-section = {
--         fresh-contents
--     }<fresh section linebreak>
--     <EOF>
--
-- (With some syntactic elements highlighted through <bracket descriptions>, and the effect of
-- `freshSeparator` in <<double brackets>>.)
freshSeparator :: Int -> Text
freshSeparator = indentedLineBreak "\n"

formatEntry :: Int -> LeaderEntry -> Fragment
formatEntry  level      (LeaderEntry leader) = (formatLeader level leader, freshSeparator level)
formatEntry _level (LeaderFragment verbatim) = (verbatim,                  mempty)

formatUnitKind :: UnitKind -> Text
formatUnitKind = \case UnitArmy -> "army"; UnitNavy -> "navy"

formatUnit :: Int -> Unit -> Text
formatUnit level (Unit kind entries) = formatFreshContents [iTrim|
${formatUnitKind kind} = {${
    joinFragments . (restoredBreak:) $
        fmap (formatEntry $ succ level) entries
}}
|]
  where
    restoredBreak = (mempty, indentedLineBreak "\n" $ succ level)

formatOobEntry :: Int -> OobEntry -> Fragment
formatOobEntry  level     (OobLeader leader) = (formatLeader level leader, freshSeparator level)
formatOobEntry  level         (OobUnit unit) = (formatUnit   level   unit, freshSeparator level)
formatOobEntry _level (OobFragment verbatim) = (verbatim,                  mempty)

formatOob :: [OobEntry] -> Text
formatOob =
    -- If the final fragment is fresh content, it ends in a newline. Otherwise we end in an original
    -- blob. Consequently the output is a text file so long as the original file was.
    joinMap (formatOobEntry 0)
