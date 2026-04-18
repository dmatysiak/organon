module Organon.Syl.Check
  ( -- * Types
    Diagnostic (..),
    Severity (..),
    CheckedProof (..),
    CheckResult (..),
    HoverItem (..),
    DefinitionItem (..),
    SwapAction (..),
    HoleFill (..),
    HoleFillEdit (..),
    ReduceAction (..),
    ExternalContext (..),
    NamespaceEntry (..),

    -- * Checking
    checkDocument,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Organon.Syl.Document
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), Solution (..), SolutionProp (..), SyllogismH (..), TermH (..), solve)
import Organon.Syl.Pretty (figureLabels, prettyFigure, prettyMoodForm, prettyMood, prettyProof, prettyProposition, prettyPropositionH, prettyRefModifier, prettySolutionProp, showText)
import Organon.Syl.Proof (reduce, reducedSyllogism)
import Organon.Syl.Tradition (MoodSpec (..), moodSpec)
import Organon.Syl.Types
import Organon.Syl.Validity

-- | Severity of a diagnostic.
data Severity = Error | Warning
  deriving stock (Eq, Show)

-- | A diagnostic message attached to a source span.
data Diagnostic = Diagnostic
  { diagStart :: SrcPos,
    diagEnd :: SrcPos,
    diagSeverity :: Severity,
    diagMessage :: Text
  }
  deriving stock (Eq, Show)

-- | A successfully checked proof.
data CheckedProof = CheckedProof
  { checkedName :: Text,
    checkedMood :: Mood,
    checkedSyllogism :: Syllogism,
    checkedSwapped :: Bool,
    checkedSteps :: [ProofStep]
  }
  deriving stock (Eq, Show)

-- | A source span with hover text.
data HoverItem = HoverItem
  { hoverStart :: SrcPos,
    hoverEnd :: SrcPos,
    hoverText :: Text
  }
  deriving stock (Eq, Show)

-- | Maps an @reference span to the definition span of the proof it refers to.
-- If defTargetFile is Nothing, the definition is in the same file.
data DefinitionItem = DefinitionItem
  { defRefStart :: SrcPos,
    defRefEnd :: SrcPos,
    defTargetFile :: Maybe FilePath,
    defTargetStart :: SrcPos,
    defTargetEnd :: SrcPos
  }
  deriving stock (Eq, Show)

-- | A code action to swap premises into canonical order.
data SwapAction = SwapAction
  { swapPrem1Start :: SrcPos,
    swapPrem1End :: SrcPos,
    swapPrem2Start :: SrcPos,
    swapPrem2End :: SrcPos
  }
  deriving stock (Eq, Show)

-- | A single text edit within a hole fill.
data HoleFillEdit = HoleFillEdit
  { fillEditStart :: SrcPos,
    fillEditEnd :: SrcPos,
    fillEditText :: Text
  }
  deriving stock (Eq, Show)

-- | A code action to fill holes with a solved proposition.
data HoleFill = HoleFill
  { holeFillMood :: Mood,
    holeFillEdits :: [HoleFillEdit],
    holeFillLabel :: Text
  }
  deriving stock (Eq, Show)

-- | A code action to reduce a syllogism to its Figure I (perfect) form.
data ReduceAction = ReduceAction
  { reducePrem1Start :: SrcPos,
    reducePrem1End   :: SrcPos,
    reducePrem2Start :: SrcPos,
    reducePrem2End   :: SrcPos,
    reduceConcStart  :: SrcPos,
    reduceConcEnd    :: SrcPos,
    reduceMood       :: Mood,
    reduceResult     :: Syllogism,
    reducePrem1Text  :: Text,
    reducePrem2Text  :: Text,
    reduceConcText   :: Text
  }
  deriving stock (Eq, Show)

-- | Per-namespace entry in the external context.
data NamespaceEntry = NamespaceEntry
  { nsFilePath    :: FilePath
  , nsConclusions :: Map Text Proposition
  , nsLocations   :: Map Text (SrcPos, SrcPos)
  }
  deriving stock (Show)

-- | External context: namespace name → namespace entry.
newtype ExternalContext = ExternalContext
  { unExternalContext :: Map Text NamespaceEntry }
  deriving stock (Show)

-- | Result of checking an entire document.
data CheckResult = CheckResult
  { checkDiagnostics :: [Diagnostic],
    checkProofs :: [CheckedProof],
    checkHovers :: [HoverItem],
    checkDefinitions :: [DefinitionItem],
    checkSwaps :: [SwapAction],
    checkHoleFills :: [HoleFill],
    checkReduces :: [ReduceAction]
  }
  deriving stock (Eq, Show)

-- | Accumulator for the document-checking loop.
data CheckAcc = CheckAcc
  { accDiags   :: [Diagnostic]
  , accProofs  :: [CheckedProof]
  , accHovers  :: [HoverItem]
  , accDefs    :: [DefinitionItem]
  , accSwaps   :: [SwapAction]
  , accFills   :: [HoleFill]
  , accReduces :: [ReduceAction]
  }
  deriving stock (Show)

emptyAcc :: CheckAcc
emptyAcc = CheckAcc [] [] [] [] [] [] []

-- | Check a parsed document: resolve references, validate syllogisms,
-- and collect diagnostics.
checkDocument :: ExternalContext -> Document -> CheckResult
checkDocument ext doc =
  let tradition = maybe Full locValue (docTradition doc)
      opens = map locValue (docOpens doc)
      -- Diagnose unknown open directives.
      openDiags =
        [ Diagnostic
            (locStart lo)
            (locEnd lo)
            Error
            ("Unknown namespace: " <> locValue lo)
          | lo <- docOpens doc,
            not (Map.member (locValue lo) (unExternalContext ext))
        ]
      acc = go tradition opens Map.empty Map.empty emptyAcc (docProofs doc)
   in CheckResult
        (openDiags ++ reverse (accDiags acc))
        (reverse (accProofs acc))
        (reverse (accHovers acc))
        (reverse (accDefs acc))
        (reverse (accSwaps acc))
        (reverse (accFills acc))
        (reverse (accReduces acc))
  where
    go _ _ _ _ acc [] = acc
    go trad opens ctx locs acc (lp : rest) =
      let block = locValue lp
          name = locValue (proofName block)
          nameStart = locStart (proofName block)
          nameEnd = locEnd (proofName block)
          -- Reference hovers and definitions are independent of proof validity.
          refHovers = mkRefHovers ext opens ctx (proofPremises block)
          refDefs = mkRefDefs ext opens locs (proofPremises block)
       in case checkProofBlock trad ext opens ctx block of
            Left (newDiags, newFills) ->
              go trad opens ctx locs
                acc { accDiags  = newDiags ++ accDiags acc
                    , accHovers = refHovers ++ accHovers acc
                    , accDefs   = refDefs ++ accDefs acc
                    , accFills  = newFills ++ accFills acc
                    }
                rest
            Right checked ->
              let concl = conclusion (checkedSyllogism checked)
                  ctx' = Map.insert name concl ctx
                  locs' = Map.insert name (nameStart, nameEnd) locs
                  proofHover = mkProofHover nameStart nameEnd checked
                  newSwaps = mkSwapAction checked (proofPremises block)
                  newReduces = mkReduceAction checked (proofPremises block) (proofConclusion block)
               in if Map.member name ctx
                    then
                      let d =
                            Diagnostic
                              nameStart
                              nameEnd
                              Error
                              ("Duplicate proof name: " <> name)
                       in go trad opens ctx locs
                            acc { accDiags = d : accDiags acc }
                            rest
                    else go trad opens ctx' locs'
                           acc { accProofs = checked : accProofs acc
                               , accHovers = refHovers ++ (proofHover : accHovers acc)
                               , accDefs   = refDefs ++ accDefs acc
                               , accSwaps  = newSwaps ++ accSwaps acc
                               , accReduces = newReduces ++ accReduces acc
                               }
                           rest

-- | Check a single proof block against the accumulated context.
checkProofBlock ::
  Tradition ->
  ExternalContext ->
  [Text] ->
  Map Text Proposition ->
  ProofBlock ->
  Either ([Diagnostic], [HoleFill]) CheckedProof
checkProofBlock trad ext opens ctx block =
  case resolvePremises ext opens ctx (proofPremises block) of
    Left diags -> Left (diags, [])
    Right resolved ->
      case resolved of
        [p1h, p2h] ->
          let conclH = locValue (proofConclusion block)
           in case (fromConcreteH p1h, fromConcreteH p2h, fromConcreteH conclH) of
                (Just p1, Just p2, Just concl) ->
                  -- All concrete: validate normally.
                  let syl = Syllogism p1 p2 concl
                   in case validate trad syl of
                        Valid mood ->
                          let steps = reduce mood syl
                           in Right (CheckedProof (locValue (proofName block)) mood syl False steps)
                        ValidSwapped mood swapped ->
                          let steps = reduce mood swapped
                           in Right (CheckedProof (locValue (proofName block)) mood swapped True steps)
                        Invalid msg ->
                          let s = locStart (proofConclusion block)
                              e = locEnd (proofConclusion block)
                           in Left ([Diagnostic s e Error msg], [])
                _ ->
                  -- At least one hole: run solver.
                  let sylH = SylH p1h p2h conclH
                      solutions = solve trad sylH
                      premLocs = proofPremises block
                      fills = map (mkSolutionFill premLocs p1h p2h conclH (proofConclusion block)) solutions
                      -- Collect all hole spans for the diagnostic.
                      holeSpans = collectHoleSpans premLocs p1h p2h conclH (proofConclusion block)
                      diags =
                        [ Diagnostic
                            s
                            e
                            Warning
                            (showText (length fills) <> " solution(s) available")
                          | (s, e) <- holeSpans
                        ]
                   in Left (diags, fills)
        _ ->
          let s = locStart (proofName block)
              e = locEnd (proofName block)
              n = length resolved
           in Left
                ( [ Diagnostic s e Error $
                      "Expected 2 premises, got " <> showText n
                  ],
                  []
                )

-- | Build a HoleFill from a solution, creating edits for each holey position.
mkSolutionFill ::
  [Located Premise] ->
  PropositionH ->
  PropositionH ->
  PropositionH ->
  Located PropositionH ->
  Solution ->
  HoleFill
mkSolutionFill [loc1, loc2] p1h p2h conclH conclLoc sol =
  let spec = moodSpec (solutionMood sol)
      (majLabels, minLabels, conLabels) = figureLabels (moodFigure spec)
      edits =
        concat
          [ mkHoleEdits (locStart loc1) (locEnd loc1) p1h (solMajor sol) majLabels,
            mkHoleEdits (locStart loc2) (locEnd loc2) p2h (solMinor sol) minLabels,
            mkHoleEdits (locStart conclLoc) (locEnd conclLoc) conclH (solConclusion sol) conLabels
          ]
      label = prettySolutionProp (solConclusion sol) conLabels
   in HoleFill (solutionMood sol) edits label
mkSolutionFill _ _ _ _ _ _ = HoleFill Barbara [] ""

-- | Create fill edits for a span if the proposition has holes.
mkHoleEdits :: SrcPos -> SrcPos -> PropositionH -> SolutionProp -> (Text, Text) -> [HoleFillEdit]
mkHoleEdits s e propH solProp labels
  | hasHoles propH = [HoleFillEdit s e (prettySolutionProp solProp labels)]
  | otherwise = []

-- | Check whether a PropositionH contains any holes.
hasHoles :: PropositionH -> Bool
hasHoles WholePropH = True
hasHoles (PropH (ConcretePT _) (ConcreteT _) (ConcreteT _)) = False
hasHoles (PropH _ _ _) = True

-- | Collect spans with holes for diagnostic placement.
collectHoleSpans ::
  [Located Premise] ->
  PropositionH ->
  PropositionH ->
  PropositionH ->
  Located PropositionH ->
  [(SrcPos, SrcPos)]
collectHoleSpans [loc1, loc2] p1h p2h conclH conclLoc =
  concat
    [ [(locStart loc1, locEnd loc1) | hasHoles p1h],
      [(locStart loc2, locEnd loc2) | hasHoles p2h],
      [(locStart conclLoc, locEnd conclLoc) | hasHoles conclH]
    ]
collectHoleSpans _ _ _ _ _ = []

-- | Convert a concrete proposition to a hole-enabled proposition.
propToH :: Proposition -> PropositionH
propToH (Proposition pt s p) = PropH (ConcretePT pt) (ConcreteT s) (ConcreteT p)

-- | Resolve premise references and holes against the local and external context.
-- Returns PropositionH for each premise: concrete ones become fully-specified PropH,
-- holes stay as-is.
resolvePremises ::
  ExternalContext ->
  [Text] ->
  Map Text Proposition ->
  [Located Premise] ->
  Either [Diagnostic] [PropositionH]
resolvePremises ext opens ctx = traverse (resolveSingle ext opens ctx)

-- | Resolve a single premise to a PropositionH.
resolveSingle ::
  ExternalContext ->
  [Text] ->
  Map Text Proposition ->
  Located Premise ->
  Either [Diagnostic] PropositionH
resolveSingle _ _ _ lp | PremiseProp prop <- locValue lp = Right (propToH prop)
resolveSingle _ _ _ lp | PremiseHole propH <- locValue lp = Right propH
resolveSingle ext opens ctx lp | PremiseRef Nothing name mmod <- locValue lp =
  case Map.lookup name ctx of
    Just prop -> applyRefModifier lp mmod prop
    Nothing ->
      let hits =
            [ prop
              | ns <- opens,
                Just entry <- [Map.lookup ns (unExternalContext ext)],
                Just prop <- [Map.lookup name (nsConclusions entry)]
            ]
       in case hits of
            [prop] -> applyRefModifier lp mmod prop
            (_ : _ : _) ->
              Left
                [ Diagnostic
                    (locStart lp)
                    (locEnd lp)
                    Error
                    ( "Ambiguous reference: @"
                        <> name
                        <> " (found in multiple opened namespaces)"
                    )
                ]
            [] ->
              Left
                [ Diagnostic
                    (locStart lp)
                    (locEnd lp)
                    Error
                    ("Unknown reference: @" <> name)
                ]
resolveSingle ext _ _ lp | PremiseRef (Just ns) name mmod <- locValue lp =
  case Map.lookup ns (unExternalContext ext) of
    Nothing ->
      Left
        [ Diagnostic
            (locStart lp)
            (locEnd lp)
            Error
            ("Unknown namespace: " <> ns)
        ]
    Just entry ->
      case Map.lookup name (nsConclusions entry) of
        Just prop -> applyRefModifier lp mmod prop
        Nothing ->
          Left
            [ Diagnostic
                (locStart lp)
                (locEnd lp)
                Error
                ("Unknown reference: @" <> ns <> "." <> name)
            ]
resolveSingle _ _ _ _ = Left []

-- | Apply a reference modifier to a resolved proposition.
-- Simple conversion is valid for E and I; conversion per accidens for A and E.
applyRefModifier :: Located Premise -> Maybe RefModifier -> Proposition -> Either [Diagnostic] PropositionH
applyRefModifier _ Nothing prop = Right (propToH prop)
applyRefModifier lp (Just RefConv) prop
  | propType prop `elem` [E, I] =
      Right (propToH (Proposition (propType prop) (predicate prop) (subject prop)))
  | otherwise =
      Left
        [ Diagnostic
            (locStart lp)
            (locEnd lp)
            Error
            ("Cannot apply simple conversion to " <> showText (propType prop) <> " proposition")
        ]
applyRefModifier lp (Just RefPerAccidens) prop
  | propType prop == A =
      Right (propToH (Proposition I (predicate prop) (subject prop)))
  | propType prop == E =
      Right (propToH (Proposition O (predicate prop) (subject prop)))
  | otherwise =
      Left
        [ Diagnostic
            (locStart lp)
            (locEnd lp)
            Error
            ("Cannot apply conversion per accidens to " <> showText (propType prop) <> " proposition")
        ]

-- | Build hover text for a proof name span.
mkProofHover :: SrcPos -> SrcPos -> CheckedProof -> HoverItem
mkProofHover s e cp =
  let mood = checkedMood cp
      syl = checkedSyllogism cp
      fig = figure syl
      spec = moodSpec mood
      triple = showText (majorPropType spec) <> showText (minorPropType spec) <> showText (conclusionPropType spec)
      figText = maybe "" (\f -> "Figure " <> prettyFigure f <> ", ") fig
      figNum = maybe "" (\f -> "-" <> prettyFigure f) fig
      swapNote = if checkedSwapped cp then " (premises swapped)" else ""
      header = figText <> prettyMood mood <> " (" <> triple <> figNum <> ")" <> swapNote <> "\n" <> prettyMoodForm mood
      body = prettyProof mood (checkedSteps cp)
   in HoverItem s e (header <> "\n\n" <> body)

-- | Build hover items for @reference premises.
mkRefHovers :: ExternalContext -> [Text] -> Map Text Proposition -> [Located Premise] -> [HoverItem]
mkRefHovers ext opens ctx = concatMap go
  where
    go lp = case locValue lp of
      PremiseRef mns name mmod ->
        case resolveRef ext opens ctx mns name of
          Just prop ->
            let base = prettyProposition prop
                converted = case mmod of
                  Nothing -> base
                  Just _ -> case applyRefModifier lp mmod prop of
                    Right ph -> base <> "\n→ " <> prettyPropositionH ph
                    Left _ -> base
             in [HoverItem (locStart lp) (locEnd lp) converted]
          Nothing -> []
      PremiseProp _ -> []
      PremiseHole _ -> []

-- | Build definition items for @reference premises.
mkRefDefs :: ExternalContext -> [Text] -> Map Text (SrcPos, SrcPos) -> [Located Premise] -> [DefinitionItem]
mkRefDefs ext opens locs = concatMap go
  where
    go lp = case locValue lp of
      PremiseRef Nothing name _ ->
        -- Try local first, then opened namespaces.
        case Map.lookup name locs of
          Just (ts, te) -> [DefinitionItem (locStart lp) (locEnd lp) Nothing ts te]
          Nothing ->
            let hits =
                  [ (nsFilePath entry, ts, te)
                    | ns <- opens,
                      Just entry <- [Map.lookup ns (unExternalContext ext)],
                      Just (ts, te) <- [Map.lookup name (nsLocations entry)]
                  ]
             in case hits of
                  [(fp, ts, te)] -> [DefinitionItem (locStart lp) (locEnd lp) (Just fp) ts te]
                  _ -> []
      PremiseRef (Just ns) name _ ->
        case Map.lookup ns (unExternalContext ext) of
          Just entry ->
            case Map.lookup name (nsLocations entry) of
              Just (ts, te) -> [DefinitionItem (locStart lp) (locEnd lp) (Just (nsFilePath entry)) ts te]
              Nothing -> []
          Nothing -> []
      PremiseProp _ -> []
      PremiseHole _ -> []

-- | Resolve a reference (qualified or unqualified) to a proposition.
resolveRef :: ExternalContext -> [Text] -> Map Text Proposition -> Maybe Text -> Text -> Maybe Proposition
resolveRef ext opens ctx Nothing name =
  case Map.lookup name ctx of
    Just prop -> Just prop
    Nothing ->
      let hits =
            [ prop
              | ns <- opens,
                Just entry <- [Map.lookup ns (unExternalContext ext)],
                Just prop <- [Map.lookup name (nsConclusions entry)]
            ]
       in case hits of
            [prop] -> Just prop
            _ -> Nothing
resolveRef ext _ _ (Just ns) name =
  case Map.lookup ns (unExternalContext ext) of
    Just entry -> Map.lookup name (nsConclusions entry)
    Nothing -> Nothing

-- | Emit a swap action if the proof required premise swapping.
mkSwapAction :: CheckedProof -> [Located Premise] -> [SwapAction]
mkSwapAction cp prems
  | checkedSwapped cp,
    [p1, p2] <- prems =
      [SwapAction (locStart p1) (locEnd p1) (locStart p2) (locEnd p2)]
  | otherwise = []

-- | Emit a reduce action for non-Figure-I syllogisms that can be reduced.
mkReduceAction :: CheckedProof -> [Located Premise] -> Located PropositionH -> [ReduceAction]
mkReduceAction cp prems conclLoc
  | [p1, p2] <- prems,
    Just fig1 <- reducedSyllogism (checkedMood cp) (checkedSyllogism cp) =
      let ops = reduceOps (checkedMood cp)
          (origMaj, origMin) = if checkedSwapped cp then (p2, p1) else (p1, p2)
          (out1, op1, out2, op2) = if opMutated ops
            then (origMin, opMinor ops, origMaj, opMajor ops)
            else (origMaj, opMajor ops, origMin, opMinor ops)
       in [ ReduceAction
              (locStart p1) (locEnd p1)
              (locStart p2) (locEnd p2)
              (locStart conclLoc) (locEnd conclLoc)
              (checkedMood cp)
              fig1
              (renderPremText op1 (locValue out1) (major fig1))
              (renderPremText op2 (locValue out2) (minor fig1))
              (prettyProposition (conclusion fig1))
          ]
  | otherwise = []

-- | What conversion a reduction applies to an original premise.
data PremOp = PremNoOp | PremSimpleConv | PremPerAccidens

-- | Describes the premise operations for a mood's reduction.
data ReduceOps = ReduceOps
  { opMajor  :: PremOp
  , opMinor  :: PremOp
  , opMutated :: Bool
  }

-- | Map a mood to the operations applied to its premises during reduction.
reduceOps :: Mood -> ReduceOps
-- Figure II
reduceOps Cesare    = ReduceOps PremSimpleConv PremNoOp        False
reduceOps Camestres = ReduceOps PremNoOp       PremSimpleConv  True
reduceOps Festino   = ReduceOps PremSimpleConv PremNoOp        False
-- Figure III
reduceOps Darapti   = ReduceOps PremNoOp       PremPerAccidens False
reduceOps Disamis   = ReduceOps PremSimpleConv PremNoOp        True
reduceOps Datisi    = ReduceOps PremNoOp       PremSimpleConv  False
reduceOps Felapton  = ReduceOps PremNoOp       PremPerAccidens False
reduceOps Ferison   = ReduceOps PremNoOp       PremSimpleConv  False
-- Figure IV
reduceOps Bramantip = ReduceOps PremNoOp       PremNoOp        True
reduceOps Camenes   = ReduceOps PremNoOp       PremNoOp        True
reduceOps Dimaris   = ReduceOps PremNoOp       PremNoOp        True
reduceOps Fesapo    = ReduceOps PremSimpleConv PremPerAccidens False
reduceOps Fresison  = ReduceOps PremSimpleConv PremSimpleConv  False
-- Subaltern moods
reduceOps Barbari   = ReduceOps PremNoOp       PremNoOp        False
reduceOps Celaront  = ReduceOps PremNoOp       PremNoOp        False
reduceOps Cesaro    = reduceOps Cesare
reduceOps Camestrop = reduceOps Camestres
reduceOps Calemos   = reduceOps Camenes
-- Figure I & reductio: not reachable (reducedSyllogism returns Nothing)
reduceOps _         = ReduceOps PremNoOp PremNoOp False

-- | Render a reference with an optional modifier.
prettyPremRef :: Maybe Text -> Text -> Maybe RefModifier -> Text
prettyPremRef mns name mmod =
  "@" <> maybe "" (<> ".") mns <> name
    <> maybe "" (\m -> " " <> prettyRefModifier m) mmod

-- | Compute the replacement text for a premise position.
-- If the original premise is a reference (with no existing modifier) and the
-- reduction applies a conversion, emit @ref conv / @ref per-accidens.
-- Otherwise fall back to the raw proposition text.
renderPremText :: PremOp -> Premise -> Proposition -> Text
renderPremText PremNoOp (PremiseRef mns name mmod) _ =
  prettyPremRef mns name mmod
renderPremText PremSimpleConv (PremiseRef mns name Nothing) _ =
  prettyPremRef mns name (Just RefConv)
renderPremText PremPerAccidens (PremiseRef mns name Nothing) _ =
  prettyPremRef mns name (Just RefPerAccidens)
renderPremText _ _ prop = prettyProposition prop
