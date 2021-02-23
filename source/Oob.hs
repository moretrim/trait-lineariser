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

leaderType :: MonadParsec errors Text parser
           => parser LeaderKind
leaderType =
        LeaderGeneral <$ keyword "land"
    <|> LeaderAdmiral <$ keyword "sea"

leader :: MonadParsec errors Text parser
       => parser Leader
leader = section "leader" . runPermutation $
        Leader
    <$> leaderEntry        "name" identifier
    <*> leaderEntry'    "picture"     scalar
    <*> leaderEntry        "date"       date
    <*> leaderEntry        "type" leaderType
    <*> leaderEntry "personality" identifier
    <*> leaderEntry  "background" identifier
    <*> leaderEntry'   "prestige"    decimal

  where
    leaderEntry  key value = toPermutation $ entry key value
    leaderEntry' key value = toPermutationWithDefault Nothing $ Just <$> entry key value

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
        { _leaderPersonality = Hardcoded.unitPersonalityIdentifier
        , _leaderBackground =
            _leaderPersonality leader `Hardcoded.productIdentifier` _leaderBackground leader
        }

------------
-- Format --
------------

-- We shut up a couple spurious “defined but not used” warnings, which seem to be caused by the
-- quasi-quotes.
formatLeader :: Int -> Leader -> Text
formatLeader level leader = [iTrim|
leader = ${
    formatLines level $ catMaybes [
        entry  "name"        (formatIdentifier . _leaderName),
        entry' "picture"     _leaderPicture,
        entry  "date"        _leaderDate,
        entry  "type"        (formatLeaderKind . _leaderKind),
        entry  "personality" (formatIdentifier . _leaderPersonality),
        entry  "background"  (formatIdentifier . _leaderBackground),
        entry' "prestige"    (fmap formatPrestige . _leaderPrestige)
    ]}
|]
  where
    formatPrestige = Text.pack . show @Decimal -- the instance results in a round-trip
    formatPair name value = [i|${name::Text} = ${value}|] :: Text
    entry  name focus = pure $ formatPair name     (focus leader)
    entry' name focus =        formatPair name <$>  focus leader

formatEntry :: Int -> LeaderEntry -> Text
formatEntry _level (LeaderFragment verbatim) =     verbatim
formatEntry  level      (LeaderEntry leader) = formatLeader level leader

formatUnit :: Int -> Unit -> Text
formatUnit level (Unit kind entries) = [iTrim|
${formatUnitKind kind} = ${
    formatLines level $ fmap (formatEntry $ succ level) entries }
|]

formatOobEntry :: Int -> OobEntry -> Text
formatOobEntry  level     (OobLeader leader) = formatLeader level leader <> "\n\n"
formatOobEntry  level         (OobUnit unit) =   formatUnit level   unit
formatOobEntry _level (OobFragment verbatim) =     verbatim

formatOob :: [OobEntry] -> Text
formatOob = Text.concat . fmap (formatOobEntry 0)
