module Organon.Syl.Tradition
  ( MoodSpec (..),
    moodSpec,
    validMoods,
    requiresExistentialImport,
    isSubaltern,
  )
where

import Organon.Syl.Types

-- | The specification of a mood: its figure and the PropType triple
--   (major premise, minor premise, conclusion).
data MoodSpec = MoodSpec
  { moodFigure :: Figure,
    majorPropType :: PropType,
    minorPropType :: PropType,
    conclusionPropType :: PropType
  }
  deriving stock (Eq, Ord, Show)

-- | Map each mood to its figure and PropType triple.
moodSpec :: Mood -> MoodSpec
-- Figure I
moodSpec Barbara = MoodSpec FigI A A A
moodSpec Celarent = MoodSpec FigI E A E
moodSpec Darii = MoodSpec FigI A I I
moodSpec Ferio = MoodSpec FigI E I O
-- Figure II
moodSpec Cesare = MoodSpec FigII E A E
moodSpec Camestres = MoodSpec FigII A E E
moodSpec Festino = MoodSpec FigII E I O
moodSpec Baroco = MoodSpec FigII A O O
-- Figure III
moodSpec Darapti = MoodSpec FigIII A A I
moodSpec Disamis = MoodSpec FigIII I A I
moodSpec Datisi = MoodSpec FigIII A I I
moodSpec Felapton = MoodSpec FigIII E A O
moodSpec Bocardo = MoodSpec FigIII O A O
moodSpec Ferison = MoodSpec FigIII E I O
-- Figure IV
moodSpec Bramantip = MoodSpec FigIV A A I
moodSpec Camenes = MoodSpec FigIV A E E
moodSpec Dimaris = MoodSpec FigIV I A I
moodSpec Fesapo = MoodSpec FigIV E A O
moodSpec Fresison = MoodSpec FigIV E I O
-- Subaltern
moodSpec Barbari = MoodSpec FigI A A I
moodSpec Celaront = MoodSpec FigI E A O
moodSpec Cesaro = MoodSpec FigII E A O
moodSpec Camestrop = MoodSpec FigII A E O
moodSpec Calemos = MoodSpec FigIV A E O

-- | Does this mood require existential import to be valid?
requiresExistentialImport :: Mood -> Bool
requiresExistentialImport Darapti = True
requiresExistentialImport Felapton = True
requiresExistentialImport Bramantip = True
requiresExistentialImport Fesapo = True
requiresExistentialImport Barbara = False
requiresExistentialImport Celarent = False
requiresExistentialImport Darii = False
requiresExistentialImport Ferio = False
requiresExistentialImport Cesare = False
requiresExistentialImport Camestres = False
requiresExistentialImport Festino = False
requiresExistentialImport Baroco = False
requiresExistentialImport Disamis = False
requiresExistentialImport Datisi = False
requiresExistentialImport Bocardo = False
requiresExistentialImport Ferison = False
requiresExistentialImport Camenes = False
requiresExistentialImport Dimaris = False
requiresExistentialImport Fresison = False
requiresExistentialImport Barbari = False
requiresExistentialImport Celaront = False
requiresExistentialImport Cesaro = False
requiresExistentialImport Camestrop = False
requiresExistentialImport Calemos = False

-- | Is this a subaltern mood (weakened conclusion)?
isSubaltern :: Mood -> Bool
isSubaltern Barbari = True
isSubaltern Celaront = True
isSubaltern Cesaro = True
isSubaltern Camestrop = True
isSubaltern Calemos = True
isSubaltern Barbara = False
isSubaltern Celarent = False
isSubaltern Darii = False
isSubaltern Ferio = False
isSubaltern Cesare = False
isSubaltern Camestres = False
isSubaltern Festino = False
isSubaltern Baroco = False
isSubaltern Darapti = False
isSubaltern Disamis = False
isSubaltern Datisi = False
isSubaltern Felapton = False
isSubaltern Bocardo = False
isSubaltern Ferison = False
isSubaltern Bramantip = False
isSubaltern Camenes = False
isSubaltern Dimaris = False
isSubaltern Fesapo = False
isSubaltern Fresison = False

-- | All valid moods for a given tradition.
validMoods :: Tradition -> [Mood]
validMoods tradition = filter (isValid tradition) [minBound .. maxBound]
  where
    isValid Strict m = not (requiresExistentialImport m) && not (isSubaltern m)
    isValid Traditional m = not (isSubaltern m)
    isValid Full _m = True
