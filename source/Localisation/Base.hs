{-|

Copyright: © 2021 moretrim
Licence:   GPL-3.0-only

Inline translations.

|-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Localisation.Base
    ( nt, nt', t, hide
    , baseLocalisation
    ) where

import GHC.Exts                                    (IsString(..))

import qualified Data.HashMap.Strict as HashMap

import Types

instance IsString Translation where
    fromString = pure . fromString

-- | No translation.
nt :: Translation
nt = Nothing

nt' :: Translations
nt' = (nt, nt, nt, nt, nt, nt, nt, nt, nt, nt, nt, nt, nt)

-- | Unmodded-style translation: English, French, German, Spanish
t :: (Translation, Translation, Translation, Translation) -> Translations
t (english, french, german, spanish) =
    (english, french, german, nt, spanish, nt, nt, nt, nt, nt, nt, nt, nt)

-- | Pseudo-translation that is invisible in the UI.
hide :: Translations
hide = ("\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC", "\ESC")

-- | Base game translations. We inline them because the source `text.csv` file has per-column
-- encodings, which is a real headache—and as things are it cannot be parsed by the program. As a
-- side-benefit, this spares the user from having to pass in the base game path.
baseLocalisation :: Localisation
baseLocalisation = HashMap.fromList
    [ ("aggressive"
      , t ( "Aggressive"
          , "Agressif"
          , "Aggressiv"
          , "Agresivo") )
    , ("relentless"
      , t ( "Relentless"
          , "Implacable"
          , "Unbarmherzig"
          , "Despiadado") )
    , ("active_p"
      , t ( "Active"
          , "Actif"
          , "Aktiv"
          , "Activo") )
    , ("earnest"
      , t ( "Earnest"
          , "Sérieux"
          , "Ernsthaft"
          , "Concienzudo") )
    , ("persistant"
      , t ( "Persistent"
          , "Persévérant"
          , "Hartnäckig"
          , "Persistente") )
    , ("balanced_p"
      , t ( "Balanced"
          , "Pondéré"
          , "Ausgewogen"
          , "Equilibrado") )
    , ("cautious"
      , t ( "Cautious"
          , "Prudent"
          , "Vorsichtig"
          , "Cauto") )
    , ("coolminded"
      , t ( "Coolminded"
          , "Serein"
          , "Bedachtsam"
          , "Templado") )
    , ("inspiring"
      , t ( "Inspiring"
          , "Stimulant"
          , "Inspirierend"
          , "Inspirador") )
    , ("gallant"
      , t ( "Gallant"
          , "Courageux"
          , "Galant"
          , "Bizarro") )
    , ("arrogant"
      , t ( "Arrogant"
          , "Arrogant"
          , "Arrogant"
          , "Arrogante") )
    , ("vainglorious"
      , t ( "Vainglorious"
          , "Vaniteux"
          , "Prahlerisch"
          , "Vanaglorioso") )
    , ("brutish"
      , t ( "Brutish"
          , "Brutal"
          , "Brutal"
          , "Brutal") )
    , ("bigoted"
      , t ( "Bigoted"
          , "Sectaire"
          , "Engstirnig"
          , "Intolerante") )
    , ("impetuous"
      , t ( "Impetuous"
          , "Impétueux"
          , "Ungestüm"
          , "Impetuoso") )
    , ("polite"
      , t ( "Polite"
          , "Poli"
          , "Höflich"
          , "Educado") )
    , ("tactful"
      , t ( "Tactful"
          , "Délicat"
          , "Taktvoll"
          , "Prudente") )
    , ("diplomatic"
      , t ( "Diplomatic"
          , "Diplomate"
          , "Diplomatisch"
          , "Diplomático") )
    , ("glory_hound"
      , t ( "Glory Hound"
          , "Ambitieux"
          , "Ruhmsüchtig"
          , "Vieja gloria") )
    , ("vicious"
      , t ( "Vicious"
          , "Violent"
          , "Bösartig"
          , "Sanguinario") )
    , ("hellbent"
      , t ( "Hellbent"
          , "Acharné"
          , "Draufgänger"
          , "Impetuoso") )
    , ("deeply_religious"
      , t ( "Deeply Religious"
          , "Très croyant"
          , "Tief religiös"
          , "Muy religioso") )
    , ("imperious"
      , t ( "Imperious"
          , "Autoritaire"
          , "Herrisch"
          , "Imperioso") )
    , ("choleric"
      , t ( "Choleric"
          , "Colérique"
          , "Cholerisch"
          , "Colérico") )
    , ("fanatic"
      , t ( "Fanatic"
          , "Fanatique"
          , "Fanatisch"
          , "Fanático") )
    , ("irate"
      , t ( "Irate"
          , "Furieux"
          , "Aufbrausend"
          , "Irascible") )
    , ("wrathful"
      , t ( "Wrathful"
          , "Courroucé"
          , "Zornerfüllt"
          , "Iracundo") )
    , ("calm"
      , t ( "Calm"
          , "Calme"
          , "Ruhig"
          , "Tranquilo") )
    , ("reserved"
      , t ( "Reserved"
          , "Réservé"
          , "Reserviert"
          , "Reservado") )
    , ("harsh"
      , t ( "Harsh"
          , "Dur"
          , "Harsch"
          , "Duro") )
    , ("merciless"
      , t ( "Merciless"
          , "Impitoyable"
          , "Gnadenlos"
          , "Cruel") )
    , ("implacable"
      , t ( "Implacable"
          , "Implacable"
          , "Unerbittlich"
          , "Implacable") )
    , ("careful"
      , t ( "Careful"
          , "Attentif"
          , "Sorgsam"
          , "Cuidadoso") )
    , ("meticulous"
      , t ( "Meticulous"
          , "Méticuleux"
          , "Genau"
          , "Meticuloso") )
    , ("openminded"
      , t ( "Openminded"
          , "Ouvert d'esprit"
          , "Offen"
          , "Sin perjuicios") )
    , ("charismatic"
      , t ( "Charismatic"
          , "Charismatique"
          , "Charismatisch"
          , "Carismático") )
    , ("impulsive"
      , t ( "Impulsive"
          , "Impulsif"
          , "Impulsiv"
          , "Impulsivo") )
    , ("intuitive"
      , t ( "Intuitive"
          , "Intuitif"
          , "Intuitiv"
          , "Intuitivo") )
    , ("resourceful"
      , t ( "Resourceful"
          , "Ingénieux"
          , "Findig"
          , "Ingenioso") )
    , ("romantic"
      , t ( "Romantic"
          , "Romantique"
          , "Romantisch"
          , "Idealista") )
    , ("brash"
      , t ( "Brash"
          , "Impudent"
          , "Forsch"
          , "Desenvuelto") )
    , ("daring"
      , t ( "Daring"
          , "Hardi"
          , "Draufgängerisch"
          , "Temerario") )
    , ("soldierly"
      , t ( "Soldierly"
          , "Militaire"
          , "Soldatisch"
          , "Soldadesco") )
    , ("disciplined"
      , t ( "Disciplined"
          , "Discipliné"
          , "Diszipliniert"
          , "Disciplinado") )
    , ("unflinching"
      , t ( "Unflinching"
          , "Opiniâtre"
          , "Entschlossen"
          , "Imperturbable") )
    , ("bold"
      , t ( "Bold"
          , "Téméraire"
          , "Mutig"
          , "Atrevido") )
    , ("audacious"
      , t ( "Audacious"
          , "Audacieux"
          , "Kühn"
          , "Audaz") )
    , ("heroic"
      , t ( "Heroic"
          , "Héroïque"
          , "Heroisch"
          , "Heroico") )
    , ("stout"
      , t ( "Stout"
          , "Solide"
          , "Beherzt"
          , "Tenaz") )
    , ("stalwart"
      , t ( "Stalwart"
          , "Robuste"
          , "Robust"
          , "Inquebrantable") )
    , ("stouthearted"
      , t ( "Stouthearted"
          , "Intrépide"
          , "Beherzt"
          , "Inamovible") )
    , ("steadfast"
      , t ( "Steadfast"
          , "Inébranlable"
          , "Solide"
          , "Categórico") )
    , ("chivalrous"
      , t ( "Chivalrous"
          , "Courtois"
          , "Ritterlich"
          , "Caballeroso") )
    , ("ballsy"
      , t ( "Ballsy"
          , "Valeureux"
          , "Forsch"
          , "Con agallas") )
    , ("aweless"
      , t ( "Aweless"
          , "Casse-cou"
          , "Respektlos"
          , "Indómito") )
    , ("dauntless"
      , t ( "Dauntless"
          , "Déterminé"
          , "Unverzagt"
          , "Intrépido") )
    , ("confident"
      , t ( "Confident"
          , "Confiant"
          , "Sicher"
          , "Seguro de sí") )
    , ("manful"
      , t ( "Manful"
          , "Ardent"
          , "Männlich"
          , "Valiente") )
    , ("doughty"
      , t ( "Doughty"
          , "Vaillant"
          , "Tapfer"
          , "Aguerrido") )
    , ("gutsy"
      , t ( "Gutsy"
          , "Brave"
          , "Waghalsig"
          , "Valentón") )
    , ("spirited"
      , t ( "Spirited"
          , "Fougueux"
          , "Sprühend"
          , "Brioso") )
    , ("resolute"
      , t ( "Resolute"
          , "Résolu"
          , "Resolut"
          , "Resuelto") )
    , ("defiant"
      , t ( "Defiant"
          , "Provocateur"
          , "Trotzig"
          , "Rebelde") )
    , ("able"
      , t ( "Able"
          , "Capable"
          , "Fähig"
          , "Capaz") )
    , ("competent"
      , t ( "Competent"
          , "Compétent"
          , "Kompetent"
          , "Competente") )
    , ("expert"
      , t ( "Expert"
          , "Expert"
          , "Experte"
          , "Experto") )
    , ("smart_p"
      , t ( "Smart"
          , "Habile"
          , "Schlau"
          , "Listo") )
    , ("intelligent"
      , t ( "Intelligent"
          , "Intelligent"
          , "Intelligent"
          , "Inteligente") )
    , ("brilliant"
      , t ( "Brilliant"
          , "Brillant"
          , "Brilliant"
          , "Brillante") )
    , ("sharp"
      , t ( "Sharp"
          , "Malin"
          , "Klug"
          , "Perspicaz") )
    , ("keen"
      , t ( "Keen"
          , "Enthousiaste"
          , "Gewitzt"
          , "Agudo") )
    , ("clever"
      , t ( "Clever"
          , "Adroit"
          , "Clever"
          , "Astuto") )
    , ("skilled"
      , t ( "Skilled"
          , "Talentueux"
          , "Geschickt"
          , "Hábil") )
    , ("efficient"
      , t ( "Efficient"
          , "Efficace"
          , "Effizient"
          , "Competente") )
    , ("au_fait"
      , t ( "Au'fait"
          , "Au fait"
          , "Sicher"
          , "Enterado") )
    , ("indifferent"
      , t ( "Indifferent"
          , "Indifférent"
          , "Gleichgültig"
          , "Indiferente") )
    , ("mediocre"
      , t ( "Mediocre"
          , "Médiocre"
          , "Mittelmäßig"
          , "Mediocre") )
    , ("lackluster"
      , t ( "Lackluster"
          , "Terne"
          , "Lustlos"
          , "Deslucido") )
    , ("timid"
      , t ( "Timid"
          , "Timide"
          , "Furchtsam"
          , "Tímido") )
    , ("chickenhearted"
      , t ( "Chickenhearted"
          , "Poltron"
          , "Ängstlich"
          , "Gallina") )
    , ("lilylivered"
      , t ( "Lilylivered"
          , "Froussard"
          , "Feige"
          , "Sangre de horchata") )
    , ("soft"
      , t ( "Soft"
          , "Indolent"
          , "Weich"
          , "Blandengue") )
    , ("spineless"
      , t ( "Spineless"
          , "Mou"
          , "Rückgratlos"
          , "Acobardado") )
    , ("unmanly"
      , t ( "Unmanly"
          , "Lâche"
          , "Unmännlich"
          , "Timorato") )
    , ("yellow"
      , t ( "Yellow"
          , "Trouillard"
          , "Memme"
          , "Miedica") )
    , ("pusillanimous"
      , t ( "Pusillanimous"
          , "Pusillanime"
          , "Verzagt"
          , "Pusilánime") )
    , ("coward"
      , t ( "Coward"
          , "Couard"
          , "Kleinmütig"
          , "Cobarde") )
    , ("craven"
      , t ( "Craven"
          , "Veule"
          , "Mutlos"
          , "Cobardón") )
    , ("bastard"
      , t ( "Bastard"
          , "Goujat"
          , "Bastard"
          , "Bastardo") )
    , ("shirker"
      , t ( "Shirker"
          , "Tire-au-flanc"
          , "Drückeberger"
          , "Escaqueado") )
    , ("scared"
      , t ( "Scared"
          , "Craintif"
          , "Verängstigt"
          , "Asustado") )
    , ("maladroit"
      , t ( "Maladroit"
          , "Maladroit"
          , "Unbeholfen"
          , "Torpe") )
    , ("undisciplined"
      , t ( "Undisciplined"
          , "Indiscipliné"
          , "Undiszipliniert"
          , "Indisciplinado") )
    , ("incapable"
      , t ( "Incapable"
          , "Incapable"
          , "Unfähig"
          , "Incapaz") )
    , ("inept"
      , t ( "Inept"
          , "Inepte"
          , "Untauglich"
          , "Inepto") )
    , ("inefficient"
      , t ( "Inefficient"
          , "Inefficace"
          , "Ineffizient"
          , "Ineficaz") )
    , ("perverse"
      , t ( "Perverse"
          , "Entêté"
          , "Pervers"
          , "Perverso") )
    , ("unruly"
      , t ( "Unruly"
          , "Insoumis"
          , "Widerborstig"
          , "Desobediente") )
    , ("disorderly"
      , t ( "Disorderly"
          , "Désordonné"
          , "Unordentlich"
          , "Desordenado") )
    , ("careless"
      , t ( "Careless"
          , "Négligent"
          , "Achtlos"
          , "Pasota") )
    , ("wretched"
      , t ( "Wretched"
          , "Misérable"
          , "Jämmerlich"
          , "Mísero") )
    , ("pisspoor"
      , t ( "Pisspoor"
          , "Pitoyable"
          , "Arm"
          , "Cutre") )
    , ("unsound"
      , t ( "Unsound"
          , "Dangereux"
          , "Unzuverlässig"
          , "Inseguro") )
    , ("strange"
      , t ( "Strange"
          , "Étrange"
          , "Merkwürdig"
          , "Raro") )
    , ("shrinking_violet"
      , t ( "Shrinking Violet"
          , "Timoré"
          , "Mauerblümchen"
          , "Vergonzoso") )
    , ("war_college"
      , t ( "War College"
          , "École militaire"
          , "Militärakademie"
          , "Academia militar") )
    , ("cavalry_school"
      , t ( "Cavalry School"
          , "École de cavalerie"
          , "Kavallerieschule"
          , "Escuela de caballería") )
    , ("armchair_general"
      , t ( "Armchair General"
          , "Général de façade"
          , "Sesselgeneral"
          , "General apoltronado") )
    , ("bureaucrat_speed"
      , t ( "Bureaucrat Speed"
          , "Vitesse de bureaucrate"
          , "Bürokratische Geschwindigkeit"
          , "Chupatintas") )
    , ("generals_aide"
      , t ( "General's Aide"
          , "Aide de camp"
          , "Generalsadjutant"
          , "Ayudante de campo") )
    , ("amateur"
      , t ( "Amateur"
          , "Amateur"
          , "Amateur"
          , "Aficionado") )
    , ("madman"
      , t ( "Madman"
          , "Fou"
          , "Irrer"
          , "Loco") )
    , ("aristocrat"
      , t ( "Aristocrat"
          , "Aristocrate"
          , "Aristokrat"
          , "Aristócrata") )
    , ("drillmaster"
      , t ( "Drillmaster"
          , "Sergent instructeur"
          , "Zuchtmeister"
          , "Instructor") )
    , ("butcher"
      , t ( "Butcher"
          , "Boucher"
          , "Schlächter"
          , "Carnicero") )
    , ("exranker"
      , t ( "Exranker"
          , "Ancien militaire"
          , "Ex-Gefreiter"
          , "Ascendido desde la tropa") )
    , ("adventurer"
      , t ( "Adventurer"
          , "Aventurier"
          , "Abenteurer"
          , "Aventurero") )
    , ("debutante"
      , t ( "Debutante"
          , "Débutant"
          , "Debütant"
          , "Debutante") )
    , ("colonial_b"
      , t ( "Colonial"
          , "Colonial"
          , "Kolonial"
          , "Colonial") )
    , ("disgraced"
      , t ( "Disgraced"
          , "Déshonoré"
          , "In Ungnade Gefallener"
          , "Deshonrado") )
    , ("priest"
      , t ( "Priest"
          , "Prêtre"
          , "Priester"
          , "Sacerdote") )
    , ("rising_star"
      , t ( "Rising Star"
          , "Étoile montante"
          , "Aufsteigender Stern"
          , "Nueva promesa") )
    , ("politician"
      , t ( "Politician"
          , "Politicien"
          , "Politiker"
          , "Político") )
    , ("poet"
      , t ( "Poet"
          , "Poète"
          , "Poet"
          , "Poeta") )
    , ("cartographer"
      , t ( "Cartographer"
          , "Cartographe"
          , "Kartograf"
          , "Cartógrafo") )
    , ("professor"
      , t ( "Professor"
          , "Professeur"
          , "Professor"
          , "Profesor") )
    , ("old_school"
      , t ( "Old School"
          , "Vieille école"
          , "Alte Schule"
          , "De la vieja escuela") )
    , ("eccentric_genius"
      , t ( "Eccentric Genius"
          , "Génie excentrique"
          , "Exzentrisches Genie"
          , "Genio excéntrico") )
    , ("gifted_administrator"
      , t ( "Gifted Administrator"
          , "Administrateur doué"
          , "Begabter Administrator"
          , "Gran administrador") )
    , ("innovative_tactician"
      , t ( "Innovative Tactician"
          , "Tacticien novateur"
          , "Innovativer Taktiker"
          , "Táctico innovador") )
    , ("expert_raider"
      , t ( "Expert Raider"
          , "Commando expérimenté"
          , "Überfallexperte"
          , "Experto en asaltos") )
    , ("natural_born_leader"
      , t ( "Natural Born Leader"
          , "Chef naturel"
          , "Geborener Anführer"
          , "Líder nato") )
    , ("megalomaniac"
      , t ( "Megalomaniac"
          , "Mégalomane"
          , "Megalomane"
          , "Megalómano") )
    , ("hated"
      , t ( "Hated"
          , "Odieux"
          , "Gehasst"
          , "Odiado") )
    , ("cursed_luck"
      , t ( "Cursed Luck"
          , "Malchanceux"
          , "Vom Pech verflucht"
          , "Mala pata") )
    , ("clueless"
      , t ( "Clueless"
          , "Ignorant"
          , "Ahnungslos"
          , "Negado") )
    , ("diplomat"
      , t ( "Diplomat"
          , "Diplomate"
          , "Diplomat"
          , "Diplomático") )
    , ("powerful_friends"
      , t ( "Powerful Friends"
          , "Amis puissants"
          , "Mächtige Freunde"
          , "De padrinos poderosos") )
    , ("immoral"
      , t ( "Immoral"
          , "Immoral"
          , "Unmoralisch"
          , "Inmoral") )
    , ("womanizer"
      , t ( "Womanizer"
          , "Coureur de jupons"
          , "Schürzenjäger"
          , "Mujeriego") )
    , ("warmonger"
      , t ( "Warmonger"
          , "Belliciste"
          , "Kriegstreiber"
          , "Belicista") )
    , ("corrupt"
      , t ( "Corrupt"
          , "Corrompu"
          , "Korrupt"
          , "Corrupto") )
    , ("bootlicker"
      , t ( "Bootlicker"
          , "Lèche-bottes"
          , "Speichellecker"
          , "Pelotillero") )
    , ("sycophant"
      , t ( "Sycophant"
          , "Flagorneur"
          , "Kriecher"
          , "Adulador") )
    , ("toady"
      , t ( "Toady"
          , "Flatteur"
          , "Schleimer"
          , "Pelota") )
    , ("yesman"
      , t ( "Yesman"
          , "Béni-oui-oui"
          , "Jasager"
          , "Pelotillero") )
    , ("brownnoser"
      , t ( "Brownnoser"
          , "Lèche-cul"
          , "Stiefellecker"
          , "Obsequioso") )
    , ("spoiled"
      , t ( "Spoiled"
          , "Gâté"
          , "Verwöhnt"
          , "Malcriado") )
    , ("school_of_defense"
      , t ( "School of Defense"
          , "École de défense"
          , "Schule der Verteidigung"
          , "Escuela de la defensa") )
    , ("school_of_offense"
      , t ( "School of Offense"
          , "École d'attaque"
          , "Schule der Offensive"
          , "Escuela del ataque") )
    , ("school_of_the_bayonet"
      , t ( "School of the Bayonet"
          , "École de la baïonnette"
          , "Schule des Bajonetts"
          , "Escuela de la bayoneta") )
    , ("school_of_firepower"
      , t ( "School of Firepower"
          , "École de la puissance de feu"
          , "Schule der Feuerkraft"
          , "Escuela de la potencia de fuego") )
    , ("artillerist"
      , t ( "Artillerist"
          , "Artilleur"
          , "Artillerist"
          , "Artillero") )
    , ("uncommonly_young"
      , t ( "Uncommonly Young"
          , "Singulièrement jeune"
          , "Ungewöhnlich jung"
          , "Extraordinariamente joven") )
    , ("sucker"
      , t ( "Sucker"
          , "Gogo"
          , "Trottel"
          , "Chupóptero") )
    , ("sad_sack"
      , t ( "Sad Sack"
          , "Raté"
          , "Trauriger Sack"
          , "Negado") )
    , ("unfit"
      , t ( "Unfit"
          , "Inapte"
          , "Ungeeignet"
          , "Inútil") )
    , ("unqualified"
      , t ( "Unqualified"
          , "Non qualifié"
          , "Unqualifiziert"
          , "Inepto") )
    , ("pawn"
      , t ( "Pawn"
          , "Pion"
          , "Pfand"
          , "Títere") )
    , ("elderly"
      , t ( "Elderly"
          , "Âgé"
          , "Älter"
          , "Anciano") )
    , ("incompetent"
      , t ( "Incompetent"
          , "Incompétent"
          , "Inkompetent"
          , "Incompetente") )

    -- fun fact: actually the translation for the unit, which shares its name with the background!
    -- had to add this one separately
    , ("engineer"
      , t ( "Engineer"
          , "Sapeur"
          , "Pionier"
          , "Ingenieros") )
    ]
