module Organon.Syl.Validity
  ( ValidationResult (..),
    validate,
    identifyMood,
  )
where

import Data.List (find)
import Data.Text (Text)
import Organon.Syl.Tradition
import Organon.Syl.Types

-- | The result of validating a syllogism.
data ValidationResult
  = -- | Valid with the given mood, premises in the supplied order.
    Valid Mood
  | -- | Valid with the given mood, but only after swapping premises.
    ValidSwapped Mood Syllogism
  | Invalid Text
  deriving stock (Eq, Show)

-- | Identify the mood of a syllogism, if it matches any known mood
-- in the given tradition.
identifyMood :: Tradition -> Syllogism -> Maybe Mood
identifyMood tradition syl =
  case figure syl of
    Nothing -> Nothing
    Just fig ->
      let pt = (propType (major syl), propType (minor syl), propType (conclusion syl))
       in findMood fig pt (validMoods tradition)

-- | Validate a syllogism against a tradition.
-- Tries both premise orderings: if the original fails figure
-- detection, swaps the premises and retries.
validate :: Tradition -> Syllogism -> ValidationResult
validate tradition syl =
  case identifyMood tradition syl of
    Just m -> Valid m
    Nothing ->
      let swapped = syl {major = minor syl, minor = major syl}
       in case identifyMood tradition swapped of
            Just m -> ValidSwapped m swapped
            Nothing ->
              case (figure syl, figure swapped) of
                (Nothing, Nothing) ->
                  Invalid "Propositions do not form a well-structured syllogism (no consistent middle term)"
                _ ->
                  Invalid "Not a valid syllogism in the given tradition"

findMood :: Figure -> (PropType, PropType, PropType) -> [Mood] -> Maybe Mood
findMood fig pt = find $ \m ->
  let spec = moodSpec m
   in moodFigure spec == fig
        && (majorPropType spec, minorPropType spec, conclusionPropType spec) == pt
