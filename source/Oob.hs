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

leaderType' :: MonadParsec errors Text parser
            => parser LeaderKind
leaderType' =
        LeaderGeneral <$ keyword' "land"
    <|> LeaderAdmiral <$ keyword' "sea"

leader :: MonadParsec errors Text parser
       => parser Leader
leader = section "leader" . runPermutation $
        Leader
    <$> leaderEntry        "name" identifier'
    <*> leaderEntry'    "picture"     scalar'
    <*> leaderEntry        "date"       date'
    <*> leaderEntry        "type" leaderType'
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

formatLeaderKind :: LeaderKind -> Text
formatLeaderKind = \case LeaderGeneral -> "land"; LeaderAdmiral -> "sea"

-- We shut up a couple spurious “defined but not used” warnings, which seem to be caused by the
-- quasi-quotes.
formatLeader :: Int -> Leader -> Text
formatLeader level leader = [iTrim|
leader = ${
    formatLines level $ catMaybes [
        entry  "name"        formatIdentifier _leaderName,
        entry' "picture"     id               _leaderPicture,
        entry  "date"        id               _leaderDate,
        entry  "type"        formatLeaderKind _leaderKind,
        entry  "personality" formatIdentifier _leaderPersonality,
        entry  "background"  formatIdentifier _leaderBackground,
        entry' "prestige"    formatDecimal    _leaderPrestige
    ]}
|]
  where
    formatPair name value = [i|${name::Text} = ${value}|]
    formatAttr name formatValue attr = formatTrailCommented (formatPair name . formatValue) attr
    entry :: Text -> (item -> Text) -> (Leader -> TrailCommented item) -> Maybe Text
    entry  name formatValue focus = pure . formatAttr name formatValue  $  focus leader
    entry' :: Text -> (item -> Text) -> (Leader -> Maybe (TrailCommented item)) -> Maybe Text
    entry' name formatValue focus =        formatAttr name formatValue <$> focus leader

-- | A textual fragment together with the separator to join with the next.
type Fragment = (Text, Text)

joinFragments :: Foldable cont => cont Fragment -> Text
joinFragments = fst . foldl' joiner ("", "")
  where
    joiner (soFar, sep) (fragment, nextSep) = (soFar <> sep <> fragment, nextSep)

joinMap :: (Functor cont, Foldable cont) => (a -> Fragment) -> cont a -> Text
joinMap = (joinFragments .) . fmap

restoredBreak :: Int -> Fragment
restoredBreak level = (mempty, indentedLineBreak "\n" level)

restoredSeparator :: Int -> Text
restoredSeparator = indentedLineBreak "\n\n"

formatUnitKind :: UnitKind -> Text
formatUnitKind = \case UnitArmy -> "army"; UnitNavy -> "navy"

formatEntry :: Int -> LeaderEntry -> Fragment
formatEntry  level      (LeaderEntry leader) = (formatLeader level leader, restoredSeparator level)
formatEntry _level (LeaderFragment verbatim) = (verbatim,                  mempty)

formatUnit :: Int -> Unit -> Text
formatUnit level (Unit kind entries) = [iTrim|
${formatUnitKind kind} = ${
    formatBlock level . joinFragments . (restoredBreak (succ level):) $
        fmap (formatEntry $ succ level) entries }
|]

formatOobEntry :: Int -> OobEntry -> Fragment
formatOobEntry  level     (OobLeader leader) = (formatLeader level leader, restoredSeparator level)
formatOobEntry  level         (OobUnit unit) = (formatUnit   level   unit, restoredSeparator level)
formatOobEntry _level (OobFragment verbatim) = (verbatim,                  mempty)

formatOob :: [OobEntry] -> Text
formatOob = joinMap (formatOobEntry 0)
