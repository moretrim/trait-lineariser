{-# OPTIONS_GHC -Wno-orphans #-}
{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Trait file parsing & linearising.

|-}
module Traits
    ( Mod
    , Trait(..), Trait'
        , traitName, traitMods
    , Traits(..), Traits'
        , traitsNullPersonality
        , traitsPersonalities
        , traitsNullBackground
        , traitsBackgrounds
        , traitsLocalisationKeys
    , traitsStructure
    , lineariseTraits
    , lineariseMods
    , lineariserHeader
    , formatTraits
    ) where

import Data.Functor.Compose
import Data.Functor.Product

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import qualified Data.MultiSet as MultiSet

import qualified Data.Text as Text

import qualified Hardcoded
import Types.Parsing

-----------
-- Parse --
-----------

-- | Present two lists as one functorial value.
type BiList = Product [] []

{-# complete BiList :: Product #-}
pattern BiList :: [item] -> [item] -> BiList item
pattern BiList as bs = Pair as bs

instance Semigroup (BiList a) where
    (BiList as bs) <> (BiList xs ys) = BiList (as <> xs) (bs <> ys)

instance Monoid (BiList a) where
    mempty = BiList mempty mempty

-- | Project out the two lists.
bi :: BiList item -> ([item], [item])
bi (BiList xs ys) = (xs, ys)

-- | Trait mod, see `Trait`.
type Mod = (Identifier, Decimal)

-- | A leader trait looks like the following:
--
--     imperious = {
--         attack = 1
--         defence = -2
--         morale = 0.20
--     }
--
-- That is, it is a named collection of army/navy modifiers.
--
-- We parametrise by the mod context.
data Trait modF = Trait
    { _traitName :: Identifier
    , _traitMods :: modF (Interspersed Mod)
    }
    deriving stock (Generic)
makeLenses ''Trait

deriving instance (Show (modF (Interspersed Mod))) => Show (Trait modF)
deriving instance (Read (modF (Interspersed Mod))) => Read (Trait modF)
deriving instance (Eq   (modF (Interspersed Mod))) => Eq   (Trait modF)
deriving instance (Ord  (modF (Interspersed Mod))) => Ord  (Trait modF)

-- | Shortcut synonym.
type Trait' = Trait []

-- | The contents of a `common/traits.txt` file. Parametrised over the trait mod context as well
-- as the context for backgrounds—so as to allow for grouping, refer to `linearise`.
data Traits modF traitF = Traits
    { _traitsNullPersonality :: Trait' -- ^ The `no_personality` entry that the game expects
    , _traitsPersonalities   :: NonEmpty (Trait modF)
    , _traitsNullBackground  :: Trait' -- ^ The `no_background` entry that the game expects
    , _traitsBackgrounds     :: traitF (Trait modF)
    }
    deriving stock (Generic)
makeLenses ''Traits

-- | Shortcut synonym.
type Traits' = Traits []

deriving instance (Show (traitF (Trait modF)), Show (Trait modF)) => Show (Traits modF traitF)
deriving instance (Read (traitF (Trait modF)), Read (Trait modF)) => Read (Traits modF traitF)
deriving instance (Eq   (traitF (Trait modF)), Eq   (Trait modF)) => Eq   (Traits modF traitF)
deriving instance (Ord  (traitF (Trait modF)), Ord  (Trait modF)) => Ord  (Traits modF traitF)

-- | Functor corresponding to Λitem. [(Trait', NonEmpty item)], the container for trait pairs
-- grouped by the original personality.
type Grouped = Compose (Compose [] ((,) Trait')) NonEmpty

-- | Project localisation keys.
traitsLocalisationKeys :: Traits modF NonEmpty -> (OrderedKeys, OrderedKeys)
traitsLocalisationKeys ts = over each traitNames ( _traitsPersonalities ts
                                                 , _traitsBackgrounds ts
                                                 )
  where
    traitNames = fmap (unquote . _traitName)

modifier :: Parser Mod
modifier = numericPair

-- | Trait parser
trait :: Parser Trait'
trait = Trait <$> identifier <*> (symbol "=" *> block (many' modifier))

-- | `common/traits.txt` structure, parsed into a `Traits`.
traitsStructure :: Parser (Traits' NonEmpty)
traitsStructure = do
    optional ws

    ((nullPersonality, personalities), (nullBackground, backgrounds)) <- runPermutation $

        (,) <$> toPermutation
                (
                    scriptEntry "personality" $ do
                        (,) <$> nullTrait "personality" "no_personality" <*> traits "personality"
                )

            <*> toPermutation
                (
                    scriptEntry "background" $ do
                        (,) <$> nullTrait "background" "no_background" <*> traits "background"
                )

    -- not present in lens 4.17.1
    let filteredBy p f val = case val ^? p of
            Nothing -> pure val
            Just witness -> indexed f witness val

    let duplicates =
            toListOf (folded . filteredBy (_2 . filtered (>1)))
            . MultiSet.toOccurList
            . MultiSet.fromList
            . toListOf (folded . traitName . to unquote)
        personalities' = duplicates $ NonEmpty.cons nullPersonality personalities
        backgrounds'   = duplicates $ NonEmpty.cons nullBackground backgrounds
        duplicatesFound kind found = [iTrim|
Duplicate ${kind::String} found:
    ${Text.unpack . Text.intercalate "\n    " $ toListOf (folded . _1) found}
Duplicates are neither supported by the game nor this tool.
|]

    when (not $ null personalities') $ do
        fail $ duplicatesFound "personalities" personalities'
    when (not $ null backgrounds') $ do
        fail $ duplicatesFound "backgrounds" backgrounds'

    eof

    pure Traits
        { _traitsNullPersonality = nullPersonality
        , _traitsPersonalities = personalities
        , _traitsNullBackground = nullBackground
        , _traitsBackgrounds = backgrounds
        }

  where
    nullTrait kind key =
        (Trait <$> pure (UnquotedIdentifier key) <*> scriptEntry key (many' modifier)) <?> descr
      where
        key'  = Text.unpack key
        kind' = Text.unpack kind
        descr =
            [iTrim|mandatory special trait ‘${ key' }’ in first position of entry ‘${ kind' }’|]

    traits kind = some trait <?> [i|at least one ${kind::String} after the special null ${kind}|]

---------------
-- Linearise --
---------------

-- | Linearise all traits, combining each personality–background combination into a single
-- background and leaving only a unit personality.
lineariseTraits :: Traits' NonEmpty -> Traits BiList Grouped
lineariseTraits traits = traits
    { _traitsPersonalities = pure unitPersonality -- not really used by the code, but it’s nice to
                                                  -- be thorough
    , _traitsBackgrounds = Compose . Compose $ do
        personality <- NonEmpty.toList $ _traitsPersonalities traits
        -- group by personality
        pure . (personality,) $ do
            traitProduct personality <$> _traitsBackgrounds traits
    }
      where
        traitProduct personality background = Trait
            { _traitName =
                _traitName personality `Hardcoded.productIdentifier` _traitName background
            , _traitMods = BiList personalityHalf backgroundHalf
            }
              where
                personalityMods = _traitMods personality
                backgroundMods  = _traitMods background

                -- track overlapping mods (with an actual value) together with their combined value
                overlappingMods :: HashMap Text Decimal
                overlappingMods = HashMap.filter (/= 0) $ HashMap.intersectionWith
                    (+)
                    (HashMap.fromList $ actualMods personalityMods)
                    (HashMap.fromList $ actualMods backgroundMods)
                      where
                        actualMods = over (mapped . _1) unquote . toListOf (traverse . _Parsed)

                -- rely on list operations to preserve the original order
                personalityHalf = fmap overlapBias personalityMods
                  where
                    overlapBias (Parsed (modName, _modValue))
                        | Just combinedValue <- unquote modName `HashMap.lookup` overlappingMods =
                            Parsed (modName, combinedValue)
                    overlapBias item = item

                backgroundHalf = fmap overlapOverride backgroundMods
                  where
                    overlapOverride item@(Parsed (modName, _modValue))
                        | unquote modName `HashMap.member` overlappingMods =
                            -- this mod has already been picked by the overlap bias and we could
                            -- leave it entirely out, but we instead leave a comment for the sake of
                            -- readability
                            Comment $ "#" <> formatMod item
                    overlapOverride item = item

        unitPersonality = Trait
            { _traitName = UnquotedIdentifier "unit_personality"
            , _traitMods = mempty
            }

-- | Present linearised traits as a mapping from raw trait identifier to mods.
lineariseMods :: Traits BiList Grouped -> HashMap Text (HashMap Text Decimal)
lineariseMods = HashMap.fromList . deTrait . reTrait . deTraits
  where
    deTraits = toListOf $ traitsBackgrounds . degroup
    degroup  = _Wrapped . _Wrapped . folded . _2 . folded

    reTrait  = over (mapped . traitMods) deBi
    deBi     = uncurry (<>) . bi

    deTrait  = fmap ((,) <$> unquote . _traitName <*> deMod . _traitMods)
    deMod    = HashMap.fromList . over (mapped . _1) unquote . deParsed
    deParsed = toListOf (traverse . _Parsed)

------------
-- Format --
------------

indent :: Int -> Text
indent level = Text.replicate (level * 4) " "

indentedLineBreak :: Text -> Int -> Text
indentedLineBreak lineBreak level = lineBreak <> indent level

offset :: Text -> Int -> [Text] -> Text
offset lineBreak level = Text.concat . fmap (indentedLineBreak lineBreak level <>)

offset' :: Int -> [Text] -> Text
offset' = offset "\n"

formatBlock :: Int -> Text -> Text
formatBlock level body = [iTrim|
    {${body}${ if Text.null body then "" else indentedLineBreak "\n" level }}
|]

formatIdentifier :: Identifier -> Text
formatIdentifier (QuotedIdentifier contents)     = "\"" <> contents <> "\""
formatIdentifier (UnquotedIdentifier identifier') = identifier'

formatMod :: Interspersed Mod -> Text
formatMod (Parsed (key, value)) = [iTrim|${formatIdentifier key} = ${value}|]
formatMod (Comment comment) = comment

formatTrait :: Int -> Trait' -> Text
formatTrait level item = [iTrim|
${ formatIdentifier $ _traitName item } = ${
    formatBlock level . offset' (succ level) . fmap formatMod $ _traitMods item }
|]

formatTrait' :: Int -> Trait BiList -> Text
formatTrait' level (Trait name (bi -> (personalityMods, backgroundMods))) = [iTrim|
${ formatIdentifier $ name } = ${
    formatBlock level $
           (offset' (succ level) . toList . fmap formatMod $ personalityMods)
        <> indentedLineBreak "\n" (succ level) <> Hardcoded.productSeparator
        <> (offset' (succ level) . toList . fmap formatMod $ backgroundMods) }
|]

-- | Trait stat summary. Not to be confused with `formatTrait`.
modSummary :: Int -> [Interspersed Mod] -> Text
modSummary level mods = [iTrim|
${offset' level . fmap (("## " <>) . formatMod) $ mods}
|]

formatGroup :: Int -> (Trait', NonEmpty (Trait BiList)) -> Text
formatGroup level (groupLeader, traits) = [iTrim|
## #${formatIdentifier $ _traitName groupLeader}${
    modSummary level $ _traitMods groupLeader
}${
    offset "\n\n" level . toList $ fmap (formatTrait' level) traits
}
|]

formatGroups :: Int -> Grouped (Trait BiList) -> Text
formatGroups level (getCompose . getCompose -> groups) =
    offset "\n\n" level . toList . fmap (formatGroup level) $ groups

lineariserHeader :: Text
lineariserHeader = [iTrim|
# This linearised trait file has been automatically generated.
# <https://github.com/moretrim/trait-lineariser>
|]

formatTraits :: Traits BiList Grouped -> Text
formatTraits traits = do
    [iTrim|
${lineariserHeader}
#
# For ease of navigation every group of personality-background pairs is opened by a comment. E.g.
# when looking for the group of all "earnest" pairs, try searching for `#earnest`.

personality = {
    ${ formatTrait 1 $ _traitsNullPersonality traits }

    ## Looking for all the personalities? they have all been merged into the personality-background
    ## pairs just below. Do NOT remove this neutral element personality without leaving at least
    ## another to replace it. It serves as the stock personality for all leaders. (This is separate
    ## from the `no_personality` entry that the game expects and uses e.g. for leaderless armies and
    ## navies.)
    unit_personality = {}
}

background = {
    ${ formatTrait 1 $ _traitsNullBackground traits }${
        formatGroups 1 $ _traitsBackgrounds traits
    }
}
|]
