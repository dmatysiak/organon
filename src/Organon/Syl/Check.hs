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
    ExternalContext (..),
    NamespaceEntry (..),

    -- * Checking
    checkDocument,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Organon.Syl.Document
import Organon.Syl.Hole (PropTypeH (..), PropositionH (..), Solution (..), SolutionProp (..), SyllogismH (..), TermH (..), solve)
import Organon.Syl.Pretty (prettyFigure, prettyMood, prettyProof, prettyProposition, prettyPropositionH, showText)
import Organon.Syl.Proof (reduce)
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
    checkHoleFills :: [HoleFill]
  }
  deriving stock (Eq, Show)

-- | Accumulator for the document-checking loop.
data CheckAcc = CheckAcc
  { accDiags  :: [Diagnostic]
  , accProofs :: [CheckedProof]
  , accHovers :: [HoverItem]
  , accDefs   :: [DefinitionItem]
  , accSwaps  :: [SwapAction]
  , accFills  :: [HoleFill]
  }
  deriving stock (Show)

emptyAcc :: CheckAcc
emptyAcc = CheckAcc [] [] [] [] [] []

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
  let edits =
        concat
          [ mkHoleEdits (locStart loc1) (locEnd loc1) p1h (solMajor sol),
            mkHoleEdits (locStart loc2) (locEnd loc2) p2h (solMinor sol),
            mkHoleEdits (locStart conclLoc) (locEnd conclLoc) conclH (solConclusion sol)
          ]
      label = case solutionPropToConc (solConclusion sol) of
        Just concl -> prettyProposition concl
        Nothing -> prettyPropositionH conclH
   in HoleFill (solutionMood sol) edits label
mkSolutionFill _ _ _ _ _ _ = HoleFill Barbara [] ""

-- | Create fill edits for a span if the proposition has holes.
mkHoleEdits :: SrcPos -> SrcPos -> PropositionH -> SolutionProp -> [HoleFillEdit]
mkHoleEdits s e propH solProp
  | hasHoles propH =
      case solutionPropToConc solProp of
        Just concl -> [HoleFillEdit s e (prettyProposition concl)]
        Nothing -> []
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

-- | Convert a SolutionProp to a concrete Proposition if all terms are known.
solutionPropToConc :: SolutionProp -> Maybe Proposition
solutionPropToConc (SolutionProp pt (Just s) (Just p)) = Just (Proposition pt s p)
solutionPropToConc _ = Nothing

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
resolveSingle ext opens ctx lp | PremiseRef Nothing name <- locValue lp =
  case Map.lookup name ctx of
    Just prop -> Right (propToH prop)
    Nothing ->
      let hits =
            [ prop
              | ns <- opens,
                Just entry <- [Map.lookup ns (unExternalContext ext)],
                Just prop <- [Map.lookup name (nsConclusions entry)]
            ]
       in case hits of
            [prop] -> Right (propToH prop)
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
resolveSingle ext _ _ lp | PremiseRef (Just ns) name <- locValue lp =
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
        Just prop -> Right (propToH prop)
        Nothing ->
          Left
            [ Diagnostic
                (locStart lp)
                (locEnd lp)
                Error
                ("Unknown reference: @" <> ns <> "." <> name)
            ]
resolveSingle _ _ _ _ = Left []

-- | Build hover text for a proof name span.
mkProofHover :: SrcPos -> SrcPos -> CheckedProof -> HoverItem
mkProofHover s e cp =
  let mood = checkedMood cp
      syl = checkedSyllogism cp
      fig = figure syl
      figText = maybe "" (\f -> "Figure " <> prettyFigure f <> ", ") fig
      swapNote = if checkedSwapped cp then " (premises swapped)" else ""
      header = figText <> prettyMood mood <> swapNote
      body = prettyProof mood (checkedSteps cp)
   in HoverItem s e (header <> "\n\n" <> body)

-- | Build hover items for @reference premises.
mkRefHovers :: ExternalContext -> [Text] -> Map Text Proposition -> [Located Premise] -> [HoverItem]
mkRefHovers ext opens ctx = concatMap go
  where
    go lp = case locValue lp of
      PremiseRef mns name ->
        case resolveRef ext opens ctx mns name of
          Just prop -> [HoverItem (locStart lp) (locEnd lp) (prettyProposition prop)]
          Nothing -> []
      PremiseProp _ -> []
      PremiseHole _ -> []

-- | Build definition items for @reference premises.
mkRefDefs :: ExternalContext -> [Text] -> Map Text (SrcPos, SrcPos) -> [Located Premise] -> [DefinitionItem]
mkRefDefs ext opens locs = concatMap go
  where
    go lp = case locValue lp of
      PremiseRef Nothing name ->
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
      PremiseRef (Just ns) name ->
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
