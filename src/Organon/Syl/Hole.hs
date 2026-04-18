module Organon.Syl.Hole
  ( TermH (..),
    PropTypeH (..),
    PropositionH (..),
    SyllogismH (..),
    SolutionProp (..),
    Solution (..),
    solve,
  )
where

import qualified Data.Map.Strict as Map
import Organon.Syl.Tradition
import Organon.Syl.Types

-- | A term that may be a hole.
data TermH = ConcreteT Term | HoleT
  deriving stock (Eq, Show)

-- | A proposition type that may be a hole.
data PropTypeH = ConcretePT PropType | HolePT
  deriving stock (Eq, Show)

-- | A proposition that may contain holes at any level,
-- or be entirely unknown.
data PropositionH
  = PropH PropTypeH TermH TermH
  | WholePropH
  deriving stock (Eq, Show)

-- | A syllogism with possible holes.
data SyllogismH = SylH
  { majorH :: PropositionH,
    minorH :: PropositionH,
    conclusionH :: PropositionH
  }
  deriving stock (Eq, Show)

-- | A proposition in a solution, where terms may be unresolved.
-- 'Nothing' means the role was unconstrained (a meta-variable).
data SolutionProp = SolutionProp
  { solPropType :: PropType,
    solSubject :: Maybe Term,
    solPredicate :: Maybe Term
  }
  deriving stock (Eq, Show)

-- | A solution: a mood and the (possibly partially-filled) syllogism.
data Solution = Solution
  { solutionMood :: Mood,
    solMajor :: SolutionProp,
    solMinor :: SolutionProp,
    solConclusion :: SolutionProp
  }
  deriving stock (Eq, Show)

-- Term roles in a syllogism.
data Role = RoleS | RoleP | RoleM
  deriving stock (Eq, Ord, Show)

-- | The role assignments for each term position given a figure.
figureRoles :: Figure -> ((Role, Role), (Role, Role), (Role, Role))
figureRoles FigI = ((RoleM, RoleP), (RoleS, RoleM), (RoleS, RoleP))
figureRoles FigII = ((RoleP, RoleM), (RoleS, RoleM), (RoleS, RoleP))
figureRoles FigIII = ((RoleM, RoleP), (RoleM, RoleS), (RoleS, RoleP))
figureRoles FigIV = ((RoleP, RoleM), (RoleM, RoleS), (RoleS, RoleP))

-- | Find all valid syllogisms that match the given pattern with holes.
solve :: Tradition -> SyllogismH -> [Solution]
solve tradition (SylH majH minH conH) =
  concatMap tryMood (validMoods tradition)
  where
    tryMood mood =
      let spec = moodSpec mood
          fig = moodFigure spec
          ((majSR, majPR), (minSR, minPR), (conSR, conPR)) = figureRoles fig
       in case matchAll
            [ (majH, majorPropType spec, majSR, majPR),
              (minH, minorPropType spec, minSR, minPR),
              (conH, conclusionPropType spec, conSR, conPR)
            ]
            Map.empty of
            Nothing -> []
            Just bindings ->
              let resolve r = Map.lookup r bindings
               in [ Solution
                      mood
                      (SolutionProp (majorPropType spec) (resolve majSR) (resolve majPR))
                      (SolutionProp (minorPropType spec) (resolve minSR) (resolve minPR))
                      (SolutionProp (conclusionPropType spec) (resolve conSR) (resolve conPR))
                  ]

    matchAll [] bindings = Just bindings
    matchAll ((propH, expectedPT, subjR, predR) : rest) bindings =
      case matchProp propH expectedPT subjR predR bindings of
        Nothing -> Nothing
        Just bindings' -> matchAll rest bindings'

    matchProp WholePropH _ _ _ bindings = Just bindings
    matchProp (PropH ptH subjH predH) expectedPT subjR predR bindings =
      case ptH of
        ConcretePT pt | pt /= expectedPT -> Nothing
        _ -> matchTerm subjH subjR bindings >>= matchTerm predH predR

    matchTerm HoleT _ bindings = Just bindings
    matchTerm (ConcreteT t) role bindings =
      case Map.lookup role bindings of
        Nothing -> Just (Map.insert role t bindings)
        Just t'
          | t == t' -> Just bindings
          | otherwise -> Nothing
