{-# LANGUAGE OverloadedStrings #-}

module Organon.Syl.Pretty
  ( prettyTerm,
    prettyPropType,
    prettyProposition,
    prettySyllogism,
    prettyFigure,
    prettyMood,
    prettyProofStep,
    prettyProof,
    prettyTradition,
    prettyTermH,
    prettyPropositionH,
    prettySolution,
    prettySolutionProp,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), Solution (..), SolutionProp (..), TermH (..))
import Organon.Syl.Tradition (MoodSpec (..), moodSpec)
import Organon.Syl.Types

-- | Render a term as text, prefixing "non-" if complemented.
prettyTerm :: Term -> Text
prettyTerm (Term n False) = n
prettyTerm (Term n True) = "non-" <> n

-- | Render a proposition type as its traditional name.
prettyPropType :: PropType -> Text
prettyPropType A = "A"
prettyPropType E = "E"
prettyPropType I = "I"
prettyPropType O = "O"

-- | Render a proposition in natural language form.
--   A: "Every S is P"
--   E: "No S is P"
--   I: "Some S is P"
--   O: "Some S is not P"
prettyProposition :: Proposition -> Text
prettyProposition (Proposition A s p) =
  "Every " <> prettyTerm s <> " is " <> prettyTerm p
prettyProposition (Proposition E s p) =
  "No " <> prettyTerm s <> " is " <> prettyTerm p
prettyProposition (Proposition I s p) =
  "Some " <> prettyTerm s <> " is " <> prettyTerm p
prettyProposition (Proposition O s p) =
  "Some " <> prettyTerm s <> " is not " <> prettyTerm p

-- | Render a figure as a number.
prettyFigure :: Figure -> Text
prettyFigure FigI = "1"
prettyFigure FigII = "2"
prettyFigure FigIII = "3"
prettyFigure FigIV = "4"

-- | Render a mood name.
prettyMood :: Mood -> Text
prettyMood = T.pack . show

-- | Render a tradition name.
prettyTradition :: Tradition -> Text
prettyTradition Strict = "Strict (15 moods)"
prettyTradition Traditional = "Traditional (19 moods)"
prettyTradition Full = "Full (24 moods)"

-- | Render a syllogism.
prettySyllogism :: Syllogism -> Text
prettySyllogism (Syllogism maj min_ concl) =
  T.intercalate
    "\n"
    [ prettyProposition maj,
      prettyProposition min_,
      "∴ " <> prettyProposition concl
    ]

-- | Render a single proof step.
prettyProofStep :: ProofStep -> Text
prettyProofStep (Axiom m) =
  "Axiom: " <> prettyMood m <> " (perfect syllogism)"
prettyProofStep (SimpleConversion from to) =
  "Simple conversion: "
    <> prettyProposition from
    <> " → "
    <> prettyProposition to
prettyProofStep (ConversionPerAccidens from to) =
  "Conversion per accidens: "
    <> prettyProposition from
    <> " → "
    <> prettyProposition to
prettyProofStep MutatePremises =
  "Mutate: swap major and minor premises"
prettyProofStep (ReductioAdImpossibile target assumed _) =
  "Reductio ad impossibile: assume "
    <> prettyProposition assumed
    <> ", derive contradiction via "
    <> prettyMood target
prettyProofStep (Subalternation from to) =
  "Subalternation: "
    <> prettyProposition from
    <> " → "
    <> prettyProposition to

-- | Render a full proof as numbered steps.
prettyProof :: Mood -> [ProofStep] -> Text
prettyProof mood steps =
  T.intercalate "\n" $
    ("Reduction of " <> prettyMood mood <> ":")
      : zipWith formatStep [(1 :: Int) ..] steps
  where
    formatStep n step =
      "  " <> T.pack (show n) <> ". " <> prettyProofStep step

-- | Render a term with possible hole.
prettyTermH :: TermH -> Text
prettyTermH (ConcreteT t) = prettyTerm t
prettyTermH HoleT = "?"

-- | Render a proposition with possible holes.
prettyPropositionH :: PropositionH -> Text
prettyPropositionH WholePropH = "?"
prettyPropositionH (PropH ptH s p_) =
  case ptH of
    ConcretePT A -> "Every " <> prettyTermH s <> " is " <> prettyTermH p_
    ConcretePT E -> "No " <> prettyTermH s <> " is " <> prettyTermH p_
    ConcretePT I -> "Some " <> prettyTermH s <> " is " <> prettyTermH p_
    ConcretePT O -> "Some " <> prettyTermH s <> " is not " <> prettyTermH p_
    HolePT -> "? " <> prettyTermH s <> " is " <> prettyTermH p_

-- | Render a term from a solution, showing "?S"/"?P"/"?M" for unresolved roles.
prettyMaybeTerm :: Maybe Term -> Text -> Text
prettyMaybeTerm (Just t) _ = prettyTerm t
prettyMaybeTerm Nothing label = label

-- | Render a solution proposition.
prettySolutionProp :: SolutionProp -> (Text, Text) -> Text
prettySolutionProp (SolutionProp A s p) (sl, pl) =
  "Every " <> prettyMaybeTerm s sl <> " is " <> prettyMaybeTerm p pl
prettySolutionProp (SolutionProp E s p) (sl, pl) =
  "No " <> prettyMaybeTerm s sl <> " is " <> prettyMaybeTerm p pl
prettySolutionProp (SolutionProp I s p) (sl, pl) =
  "Some " <> prettyMaybeTerm s sl <> " is " <> prettyMaybeTerm p pl
prettySolutionProp (SolutionProp O s p) (sl, pl) =
  "Some " <> prettyMaybeTerm s sl <> " is not " <> prettyMaybeTerm p pl

-- | Role labels for each figure position.
figureLabels :: Figure -> ((Text, Text), (Text, Text), (Text, Text))
figureLabels FigI = (("?M", "?P"), ("?S", "?M"), ("?S", "?P"))
figureLabels FigII = (("?P", "?M"), ("?S", "?M"), ("?S", "?P"))
figureLabels FigIII = (("?M", "?P"), ("?M", "?S"), ("?S", "?P"))
figureLabels FigIV = (("?P", "?M"), ("?M", "?S"), ("?S", "?P"))

-- | Render a solution: mood info + filled-in syllogism.
prettySolution :: Solution -> Text
prettySolution (Solution mood maj min_ concl) =
  let spec = moodSpec mood
      triple =
        prettyPropType (majorPropType spec)
          <> prettyPropType (minorPropType spec)
          <> prettyPropType (conclusionPropType spec)
      fig = prettyFigure (moodFigure spec)
      (majLabels, minLabels, conLabels) = figureLabels (moodFigure spec)
   in prettyMood mood
        <> " ("
        <> triple
        <> "-"
        <> fig
        <> "): "
        <> prettySolutionProp maj majLabels
        <> "; "
        <> prettySolutionProp min_ minLabels
        <> "; "
        <> "∴ "
        <> prettySolutionProp concl conLabels
