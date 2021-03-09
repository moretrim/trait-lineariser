{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Localisation parsing & linearising.

|-}
module Localisation
    ( localisations
    , extendLocalisation
    , lineariseLocalisation
    , formatLocalisation
    ) where

import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Text as Text

import qualified Hardcoded
import Types.Parsing
import Localisation.Base

entries :: [(Key, TranslationsRepresentation)] -> [Entry]
entries = coerce

localisationHeader' :: [Entry]
localisationHeader' = entries
    [ ("### This localisation file has been automatically generated ###"
      , ( "English"
        , "French"
        , "German"
        , "Polish"
        , "Spanish"
        , "Italian"
        , "Swedish"
        , "Czech"
        , "Hungarian"
        , "Dutch"
        , "Portuguese"
        , "Russian"
        , "Finnish"
        )
      )
    , ("#   <https://github.com/moretrim/trait-lineariser>            #"
      , nt')
    ]

{-
These keys appear unused by the UI:

    [ ( "UV_PERSONALITY"
      , t ( "Personality: $VAL$"
          , "Personnalité : $VAL$"
          , "Persönlichkeit: $VAL$"
          , "Personalidad: $VAL$") )
    , ( "UV_BACKGROUND"
      , t ( "Background: $VAL$"
          , "Caractéristique : $VAL$"
          , "Hintergrund: $VAL$"
          , "Historial: $VAL$") )
    ]
-}

localisationPreamble' :: [Entry]
localisationPreamble' = entries
    [ ( "## UI tweaks"
      , nt')
    , ( "MILITARY_PERSONALITY"
      , hide)
    , ( "MILITARY_BACKGROUND"
      , t ( "Personality & Background:\\n  $NAME$"
          , "Personnalité & caractéristique :\\n  $NAME$"
          , "Persönlichkeit & Hintergrund:\\n  $NAME$"
          , "Personalidad y historial:\\n  $NAME$") )
    , ( "UV_BACKGROUND_LABEL"
      , t ( "Personality & Background"
          , "Personnalité & caractéristique"
          , "Persönlichkeit & Hintergrund"
          , "Personalidad y historial") )
    , ( "DEFINE_ADMIRAL_EFFECT"
      , t ( "Gain the service of §YAdmiral $NAME$§W ($BAC$).\\n"
          , "Obtenir les services de l'§Yamiral $NAME$§W ($BAC$).\\n"
          , "§YAdmiral $NAME$§W ($BAC$) rekrutieren.\\n"
          , "Obtener los servicios del §Yalmirante $NAME$§W ($BAC$).\\n") )
    , ( "DEFINE_GENERAL_EFFECT"
      , t ( "Gain the service of §YGeneral $NAME$§W ($BAC$).\\n"
          , "Obtenir les services du §Ygénéral $NAME$§W ($BAC$).\\n"
          , "§YGeneral $NAME$§W ($BAC$) rekrutieren.\\n"
          , "Obtener los servicios del §Ygeneral $NAME$§W ($BAC$).\\n") )

    , ( "no_personality"
      , hide)
    , ( "unit_personality"
      , hide)

    , ( "no_background"
      , t ( "No Traits"
          , "Aucune caractéristique"
          , "Keine Eigenschaften"
          , "Sin rasgos") )

    , ( "## Personality x background pairs"
      , nt')
    ]

localisationHeader :: Text
localisationHeader = formatLocalisation localisationHeader'

-- | Localisation file row. Also see `localisations`.
row :: Parser Entry
row = do
    key <- keyCol

    english    <- translationCol
    french     <- translationCol
    german     <- translationCol
    polish     <- translationCol
    spanish    <- translationCol
    italian    <- translationCol
    swedish    <- translationCol
    czech      <- translationCol
    hungarian  <- translationCol
    dutch      <- translationCol
    portuguese <- translationCol
    russian    <- translationCol
    finnish    <- translationCol

    crud

    pure $ (key
           , Translations ( english
                          , french
                          , german
                          , polish
                          , spanish
                          , italian
                          , swedish
                          , czech
                          , hungarian
                          , dutch
                          , portuguese
                          , russian
                          , finnish
                          )
           )

      where
        separator     = char ';' <?> "entry separator (semicolon)"
        nonSeparators = many $ noneOf (";\n" :: String)

        key'         =                        nonSeparators  <?> "localisation key"
        translation  = optional (separator *> nonSeparators) <?> "localisation translation"

        keyCol = Text.pack <$> key'
        translationCol = do
            col <- translation
            pure $ do
                contents <- col
                guard (not $ null contents) $>
                    Text.pack contents

        -- | It seems that internally PDS terminated their rows by an 'x'. These seems to have been
        -- followed by an accumulating and inconsistent amount of commas and semicolons over time,
        -- and to have been horribly mangled multiple times.
        --
        -- (N.b. the following exceptions come from the following unmodded files:
        -- - ‘<’: beta1.csv
        -- - ‘ ’: beta2.csv
        -- - ‘.’: event_news.csv
        -- - <ETX> (!!!!!!): housedivided.csv
        -- - horizontal tab: housedivided2_1.csv
        -- - ‘X’: newtext.csv
        -- - ‘"’: newtext.csv
        --
        -- The following is from HFM:
        -- - ‘\’: FlavoursMod_events.csv
        -- - ‘!’: 00_HPM_Misc.csv (unfinished 1.28 branch)
        -- )
        crud = many (oneOf (";,xX< .\ETX\t\"\\!" :: String)) <?> "any amount of row terminators [;,x]"

-- | Victoria II’s localisation files take the form of simple semicolon-separated data, with no
-- support for quoting or need for a header.
localisations :: Parser Localisation
localisations = generatedFile <|> do
    rows <- row `sepBy` eor
    optional eor
    eof

    -- In case of duplicates, `fromList` is biased towards later entries. No idea if this reflects
    -- the behaviour of the game or not.
    pure $ HashMap.fromList rows

      where
        eor = eol <?> "end of row"
        -- skip generated file
        generatedFile = mempty <$ string localisationHeader

-- | Extend localisation with base game entries according to the `BaseGameLocalisation` setting.
extendLocalisation :: BaseGameLocalisation -> Localisation -> Localisation
extendLocalisation baseGameLocalisation localisation = case baseGameLocalisation of
    IncludeBaseGame   -> HashMap.union localisation baseLocalisation
    NoIncludeBaseGame -> localisation

-- | Compute the localisation of combining personalities and backgrounds into linear trait pairs.
-- Reports orphan keys (i.e. those that had no localisation entries) on the side.
--
-- Also uses modifier data to produce stat summaries in the translations.
lineariseLocalisation :: HashMap Key (HashMap Text Decimal)
                      -> Localisation
                      -> OrderedKeys
                      -> OrderedKeys
                      -> (HashSet.HashSet Key, OrderedLocalisation)
lineariseLocalisation linearisedTraits localisation personalities' backgrounds' =
    (orphans, productEntries)
  where
    toSet = HashSet.toMap . HashSet.fromList . toList
    orphans = HashMap.keysSet $
           HashMap.difference (toSet personalities') localisation
        <> HashMap.difference (toSet backgrounds')   localisation

    -- maintain original trait order by using listy operations, but stick to what can be translated
    translatable = NonEmpty.filter (`HashMap.member` localisation)
    personalities = translatable personalities'
    backgrounds   = translatable backgrounds'
    translation :: Key -> Translations
    translation trait =
        -- sticking to what can be translated justifies the following
        fromJust $ HashMap.lookup trait localisation

    productEntries =
        localisationHeader'
        <> localisationPreamble'
        <> toList (productEntry <$> personalities <*> backgrounds)

    productEntry :: Key -> Key -> (Key, Translations)
    productEntry personality background =
        (Hardcoded.productKey personality background, translations)
      where
        personalityTranslations, backgroundTranslations, translations :: Translations
        personalityTranslations = translation personality
        backgroundTranslations  = translation background
        translations = liftA2
            (liftA2 $ concatTraits personality background)
            personalityTranslations
            backgroundTranslations

    concatTraits :: Key -> Key -> Text -> Text -> Text
    concatTraits personality background personalityTranslation backgroundTranslation =
        Hardcoded.productTranslation personalityTranslation backgroundTranslation stats
          where
            linearisedTrait = Hardcoded.productKey personality background
            mods            = HashMap.lookup linearisedTrait linearisedTraits

            highlight = Hardcoded.colour Hardcoded.hpmPalette

            stats = (attackHighlight, defenceHighlight, speedHighlight, extrasHighlight)

            (attackMod, defenceMod, speedMod, organisationMod, moraleMod) =
                over each (\mod -> HashMap.lookup mod =<< mods)
                    ( Hardcoded.attack
                    , Hardcoded.defence
                    , Hardcoded.speed
                    , Hardcoded.organisation
                    , Hardcoded.morale
                    )

            numericHighlight blanks format importance =
                fromMaybe                 blanks . fmap (highlight <$> importance <*> format)
            extraHighlight =
                fromMaybe Hardcoded.symbolBlanks . fmap (highlight <$> id         <*> Hardcoded.importanceSymbol)

            attackHighlight  = numericHighlight Hardcoded.integralBlanks   Hardcoded.formatInteger    Hardcoded.attackImportance  attackMod
            defenceHighlight = numericHighlight Hardcoded.integralBlanks   Hardcoded.formatInteger    Hardcoded.defenceImportance defenceMod
            speedHighlight   = numericHighlight Hardcoded.percentageBlanks Hardcoded.formatPercentage Hardcoded.speedImportance   speedMod
            extrasHighlight  = extraHighlight (pure $ Hardcoded.extrasImportance organisationMod moraleMod)

formatEntry :: Entry -> Text
formatEntry (key, translations) = key <> ";" <> translationColumns
  where
    translationColumns =
        Text.concat . toList $ fmap formatColumn translations -- for want of foldMap'
    formatColumn translation = fromMaybe "" translation <> ";"

formatLocalisation :: [Entry] -> Text
formatLocalisation =
    -- `Text.unlines` ensures output is in fact a text file
    Text.unlines . fmap formatEntry
