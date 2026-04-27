module Organon.Tfl.Check
  ( -- * Types
    Diagnostic (..),
    Severity (..),
    CheckedProof (..),
    CheckResult (..),
    HoverItem (..),
    DefinitionItem (..),
    HoleFill (..),
    HoleFillEdit (..),
    ExternalContext (..),
    NamespaceEntry (..),

    -- * Checking
    checkDocument,

    -- * Reference modifiers (exported for Pretty/tests)
    applyRefModifier,
    applyConv,
    applyPerAccidens,
    applyObv,
    applyContra,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Organon.Tfl.Document
import Organon.Tfl.Hole (SignedTermH (..), StatementH (..))
import Organon.Tfl.Pretty
  ( prettyCancellation,
    prettySignedTerm,
    prettyStatement,
    prettyStatementEnglish,
  )
import Organon.Tfl.Types
import Organon.Tfl.Validity

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

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
    checkedInference :: Inference,
    checkedCancellation :: Cancellation
  }
  deriving stock (Eq, Show)

-- | A source span with hover text.
data HoverItem = HoverItem
  { hoverStart :: SrcPos,
    hoverEnd :: SrcPos,
    hoverText :: Text
  }
  deriving stock (Eq, Show)

-- | Maps a @reference span to the definition span of the proof it refers to.
data DefinitionItem = DefinitionItem
  { defRefStart :: SrcPos,
    defRefEnd :: SrcPos,
    defTargetFile :: Maybe FilePath,
    defTargetStart :: SrcPos,
    defTargetEnd :: SrcPos
  }
  deriving stock (Eq, Show)

-- | A single text edit within a hole fill.
data HoleFillEdit = HoleFillEdit
  { fillEditStart :: SrcPos,
    fillEditEnd :: SrcPos,
    fillEditText :: Text
  }
  deriving stock (Eq, Show)

-- | A code action to fill a hole conclusion with uncancelled terms.
data HoleFill = HoleFill
  { holeFillEdits :: [HoleFillEdit],
    holeFillLabel :: Text
  }
  deriving stock (Eq, Show)

-- | Per-namespace entry in the external context.
data NamespaceEntry = NamespaceEntry
  { nsFilePath :: FilePath,
    nsConclusions :: Map Text Statement,
    nsLocations :: Map Text (SrcPos, SrcPos)
  }
  deriving stock (Show)

-- | External context: namespace name → namespace entry.
newtype ExternalContext = ExternalContext
  { unExternalContext :: Map Text NamespaceEntry
  }
  deriving stock (Show)

-- | Result of checking an entire document.
data CheckResult = CheckResult
  { checkDiagnostics :: [Diagnostic],
    checkProofs :: [CheckedProof],
    checkHovers :: [HoverItem],
    checkDefinitions :: [DefinitionItem],
    checkHoleFills :: [HoleFill]
  }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Accumulator
-- ---------------------------------------------------------------------------

data CheckAcc = CheckAcc
  { accDiags :: [Diagnostic],
    accProofs :: [CheckedProof],
    accHovers :: [HoverItem],
    accDefs :: [DefinitionItem],
    accFills :: [HoleFill]
  }

emptyAcc :: CheckAcc
emptyAcc = CheckAcc [] [] [] [] []

-- ---------------------------------------------------------------------------
-- Main entry point
-- ---------------------------------------------------------------------------

-- | Check a parsed TFL document: resolve references, validate inferences,
-- and collect diagnostics.
checkDocument :: ExternalContext -> Document -> CheckResult
checkDocument ext doc =
  let opens = map locValue (docOpens doc)
      rl = docRelLexicon doc
      openDiags =
        [ Diagnostic
            (locStart lo)
            (locEnd lo)
            Error
            ("Unknown namespace: " <> locValue lo)
          | lo <- docOpens doc,
            not (Map.member (locValue lo) (unExternalContext ext))
        ]
      acc = go rl opens Map.empty Map.empty emptyAcc (docProofs doc)
   in CheckResult
        (openDiags ++ reverse (accDiags acc))
        (reverse (accProofs acc))
        (reverse (accHovers acc))
        (reverse (accDefs acc))
        (reverse (accFills acc))
  where
    go _ _ _ _ acc [] = acc
    go rl opens ctx locs acc (lp : rest) =
      let block = locValue lp
          name = locValue (proofName block)
          nameStart = locStart (proofName block)
          nameEnd = locEnd (proofName block)
          refHovers = mkRefHovers rl ext opens ctx (proofPremises block)
          refDefs = mkRefDefs ext opens locs (proofPremises block)
       in case checkProofBlock ext opens ctx block of
            Left (newDiags, newFills) ->
              go rl opens ctx locs
                acc
                  { accDiags = newDiags ++ accDiags acc,
                    accHovers = refHovers ++ accHovers acc,
                    accDefs = refDefs ++ accDefs acc,
                    accFills = newFills ++ accFills acc
                  }
                rest
            Right checked ->
              let concl = conclusion (checkedInference checked)
                  ctx' = Map.insert name concl ctx
                  locs' = Map.insert name (nameStart, nameEnd) locs
                  proofHover = mkProofHover rl nameStart nameEnd checked
               in if Map.member name ctx
                    then
                      let d =
                            Diagnostic
                              nameStart
                              nameEnd
                              Error
                              ("Duplicate proof name: " <> name)
                       in go rl opens ctx locs
                            acc {accDiags = d : accDiags acc}
                            rest
                    else
                      go rl opens ctx' locs'
                        acc
                          { accProofs = checked : accProofs acc,
                            accHovers = refHovers ++ (proofHover : accHovers acc),
                            accDefs = refDefs ++ accDefs acc
                          }
                        rest

-- ---------------------------------------------------------------------------
-- Proof block checking
-- ---------------------------------------------------------------------------

checkProofBlock ::
  ExternalContext ->
  [Text] ->
  Map Text Statement ->
  ProofBlock ->
  Either ([Diagnostic], [HoleFill]) CheckedProof
checkProofBlock ext opens ctx block =
  case resolvePremises ext opens ctx (proofPremises block) of
    Left diags -> Left (diags, [])
    Right resolved ->
      let conclH = locValue (proofConclusion block)
       in case (allConcrete resolved, fromConcreteH conclH) of
            (Just stmts, Just concl) ->
              -- All concrete: validate.
              let inf = Inference stmts concl
               in case validate inf of
                    Valid cancel ->
                      Right (CheckedProof (locValue (proofName block)) inf cancel)
                    Invalid cancel errs ->
                      let s = locStart (proofConclusion block)
                          e = locEnd (proofConclusion block)
                          diags = map (Diagnostic s e Error) errs
                       in Left (diags, [])
            (Just stmts, Nothing) ->
              -- Premises concrete, conclusion has holes: solve by computing
              -- uncancelled terms from premises.
              let cancel = cancellation stmts
                  uncancelledSTs = map snd (uncancelled cancel)
                  solvedConcl = Statement uncancelledSTs
                  conclLoc = proofConclusion block
                  fill =
                    HoleFill
                      { holeFillEdits =
                          [HoleFillEdit (locStart conclLoc) (locEnd conclLoc) (prettyStatement solvedConcl)],
                        holeFillLabel = prettyStatement solvedConcl
                      }
                  diag =
                    Diagnostic
                      (locStart conclLoc)
                      (locEnd conclLoc)
                      Warning
                      ("Solved conclusion: " <> prettyStatement solvedConcl)
               in Left ([diag], [fill])
            _ ->
              -- Check for exactly one whole-statement hole with concrete conclusion.
              case fromConcreteH conclH of
                Just concl ->
                  let (holes, concretes) = partitionHoles (zip resolved (proofPremises block))
                   in case holes of
                        [(_, holeLoc)] | length concretes == length resolved - 1 ->
                          case solvePremiseHole (map fst concretes) concl of
                            Just solved ->
                              let s = locStart holeLoc
                                  e = locEnd holeLoc
                                  fill =
                                    HoleFill
                                      { holeFillEdits = [HoleFillEdit s e (prettyStatement solved)],
                                        holeFillLabel = prettyStatement solved
                                      }
                                  diag = Diagnostic s e Warning ("Solved premise: " <> prettyStatement solved)
                               in Left ([diag], [fill])
                            Nothing -> cantValidate
                        _ -> cantValidate
                Nothing -> cantValidate
              where
                cantValidate =
                  let s = locStart (proofName block)
                      e = locEnd (proofName block)
                   in Left
                        ( [ Diagnostic s e Warning
                              "Cannot validate: premises contain holes"
                          ],
                          []
                        )

-- | Try to extract all concrete statements from resolved premises.
allConcrete :: [StatementH] -> Maybe [Statement]
allConcrete = traverse fromConcreteH

-- | Partition resolved premises into whole-statement holes and concrete
-- statements, keeping their Located premise info for source positions.
partitionHoles ::
  [(StatementH, Located Premise)] ->
  ([(StatementH, Located Premise)], [(Statement, Located Premise)])
partitionHoles = foldr go ([], [])
  where
    go (WholeStmtH, loc) (hs, cs) = ((WholeStmtH, loc) : hs, cs)
    go (sh, loc) (hs, cs) = case fromConcreteH sh of
      Just s  -> (hs, (s, loc) : cs)
      Nothing -> ((sh, loc) : hs, cs)

-- | Solve for a missing premise given concrete known premises and a
-- concrete conclusion.
solvePremiseHole :: [Statement] -> Statement -> Maybe Statement
solvePremiseHole [] concl = Just concl
solvePremiseHole known concl =
  let cancel = cancellation known
      unc = map snd (uncancelled cancel)
      conclTs = terms concl
      -- Match uncancelled terms against conclusion terms
      (usedU, usedC) = matchTerms unc conclTs
      -- Uncancelled not in conclusion -> missing premise must cancel them
      toCancel =
        [ case sign st of
            Fixed s -> Just (st {sign = Fixed (flipSign s)})
            Wild    -> Nothing
        | (st, used) <- zip unc usedU
        , not used
        ]
      -- Conclusion terms not covered -> missing premise must supply them
      toSupply = [st | (st, used) <- zip conclTs usedC, not used]
   in if any (== Nothing) toCancel
        then Nothing
        else
          let terms = map (\(Just x) -> x) toCancel ++ toSupply
           in if null terms then Nothing else Just (Statement terms)

-- | Mark which uncancelled terms match which conclusion terms (same sign, same
-- term expression and positions).
matchTerms :: [SignedTerm] -> [SignedTerm] -> ([Bool], [Bool])
matchTerms uncs concs =
  let mu = replicate (length uncs) False
      mc = replicate (length concs) False
   in foldl
        (\(u, c) i ->
          case findMatch i (uncs !! i) concs c of
            Just j  -> (setAt i True u, setAt j True c)
            Nothing -> (u, c)
        )
        (mu, mc)
        [0 .. length uncs - 1]

findMatch :: Int -> SignedTerm -> [SignedTerm] -> [Bool] -> Maybe Int
findMatch _ _ [] _ = Nothing
findMatch _ st concs used = go 0
  where
    go j
      | j >= length concs = Nothing
      | used !! j = go (j + 1)
      | signedTermMatch st (concs !! j) = Just j
      | otherwise = go (j + 1)

signedTermMatch :: SignedTerm -> SignedTerm -> Bool
signedTermMatch a b =
  sign a == sign b
    && termExpr a == termExpr b
    && positions a == positions b

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- Reference resolution
-- ---------------------------------------------------------------------------

resolvePremises ::
  ExternalContext ->
  [Text] ->
  Map Text Statement ->
  [Located Premise] ->
  Either [Diagnostic] [StatementH]
resolvePremises ext opens ctx = traverse (resolveSingle ext opens ctx)

resolveSingle ::
  ExternalContext ->
  [Text] ->
  Map Text Statement ->
  Located Premise ->
  Either [Diagnostic] StatementH
resolveSingle _ _ _ lp | PremiseStmt stmt <- locValue lp =
  Right (stmtToH stmt)
resolveSingle _ _ _ lp | PremiseHole stmtH <- locValue lp =
  Right stmtH
resolveSingle ext opens ctx lp | PremiseRef Nothing name mmod <- locValue lp =
  case Map.lookup name ctx of
    Just stmt -> applyRefModifier lp mmod stmt
    Nothing ->
      let hits =
            [ stmt
              | ns <- opens,
                Just entry <- [Map.lookup ns (unExternalContext ext)],
                Just stmt <- [Map.lookup name (nsConclusions entry)]
            ]
       in case hits of
            [stmt] -> applyRefModifier lp mmod stmt
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
        Just stmt -> applyRefModifier lp mmod stmt
        Nothing ->
          Left
            [ Diagnostic
                (locStart lp)
                (locEnd lp)
                Error
                ("Unknown reference: @" <> ns <> "." <> name)
            ]
resolveSingle _ _ _ _ = Left []

-- ---------------------------------------------------------------------------
-- Reference modifiers
-- ---------------------------------------------------------------------------

-- | Apply a reference modifier to a resolved statement.
applyRefModifier :: Located Premise -> Maybe RefModifier -> Statement -> Either [Diagnostic] StatementH
applyRefModifier _ Nothing stmt = Right (stmtToH stmt)
applyRefModifier lp (Just RefConv) stmt =
  case applyConv stmt of
    Just s -> Right (stmtToH s)
    Nothing ->
      Left
        [ Diagnostic
            (locStart lp)
            (locEnd lp)
            Error
            "Cannot apply simple conversion: requires a 2-term E or I statement (symmetric signs)"
        ]
applyRefModifier lp (Just RefPerAccidens) stmt =
  case applyPerAccidens stmt of
    Just s -> Right (stmtToH s)
    Nothing ->
      Left
        [ Diagnostic
            (locStart lp)
            (locEnd lp)
            Error
            "Cannot apply conversion per accidens: requires a 2-term A or E statement"
        ]
applyRefModifier _ (Just RefObv) stmt =
  Right (stmtToH (applyObv stmt))
applyRefModifier lp (Just RefContra) stmt =
  case applyContra stmt of
    Just s -> Right (stmtToH s)
    Nothing ->
      Left
        [ Diagnostic
            (locStart lp)
            (locEnd lp)
            Error
            "Cannot apply contraposition: requires a 2-term A or O statement"
        ]

-- | Simple conversion: swap terms. Valid for E (−S −P) and I (+S +P)
-- where signs are symmetric.
applyConv :: Statement -> Maybe Statement
applyConv (Statement [st1, st2])
  | sign st1 == sign st2 = Just (Statement [st2, st1])
applyConv _ = Nothing

-- | Conversion per accidens: swap terms and weaken.
-- A (−S +P) → I (+P +S), E (−S −P) → O (+P −S).
applyPerAccidens :: Statement -> Maybe Statement
applyPerAccidens (Statement [st1, st2])
  | sign st1 == Fixed Minus, sign st2 == Fixed Plus =
      -- A: −S +P → +P +S
      Just (Statement [SignedTerm (Fixed Plus) (termExpr st2) (positions st2), SignedTerm (Fixed Plus) (termExpr st1) (positions st1)])
  | sign st1 == Fixed Minus, sign st2 == Fixed Minus =
      -- E: −S −P → +P −S
      Just (Statement [SignedTerm (Fixed Plus) (termExpr st2) (positions st2), SignedTerm (Fixed Minus) (termExpr st1) (positions st1)])
applyPerAccidens _ = Nothing

-- | Obversion: negate the second term's sign and toggle its complement.
-- Always valid. Works on any statement by toggling the last term.
applyObv :: Statement -> Statement
applyObv (Statement []) = Statement []
applyObv (Statement sts) =
  let (prefix, lst) = (init sts, last sts)
      flipped = SignedTerm (flipWildSign (sign lst)) (complementTermExpr (termExpr lst)) (positions lst)
   in Statement (prefix ++ [flipped])

-- | Contraposition: flip all signs and complement all terms.
-- Valid for A (−S +P → −non-P +non-S) and O (+S −P → +non-P −non-S).
applyContra :: Statement -> Maybe Statement
applyContra (Statement [st1, st2])
  | isAPattern st1 st2 || isOPattern st1 st2 =
      Just
        ( Statement
            [ SignedTerm (sign st2) (complementTermExpr (termExpr st2)) (positions st2),
              SignedTerm (sign st1) (complementTermExpr (termExpr st1)) (positions st1)
            ]
        )
applyContra _ = Nothing

isAPattern :: SignedTerm -> SignedTerm -> Bool
isAPattern st1 st2 = sign st1 == Fixed Minus && sign st2 == Fixed Plus

isOPattern :: SignedTerm -> SignedTerm -> Bool
isOPattern st1 st2 = sign st1 == Fixed Plus && sign st2 == Fixed Minus

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Convert a concrete statement to a hole-enabled statement.
stmtToH :: Statement -> StatementH
stmtToH (Statement sts) = StmtH (map ConcreteSTH sts)

-- | Flip a WildSign: Plus↔Minus, Wild stays Wild.
flipWildSign :: WildSign -> WildSign
flipWildSign (Fixed s) = Fixed (flipSign s)
flipWildSign Wild = Wild

-- | Toggle a term expression's complement flag.
-- For atomic terms, toggles the complemented flag.
-- For compound terms, complements each sub-term.
complementTermExpr :: TermExpr -> TermExpr
complementTermExpr (Atomic t) = Atomic (t {complemented = not (complemented t)})
complementTermExpr (Compound sts) =
  Compound [st {termExpr = complementTermExpr (termExpr st)} | st <- sts]

showInt :: Int -> Text
showInt = T.pack . show

-- ---------------------------------------------------------------------------
-- Hover items
-- ---------------------------------------------------------------------------

-- | Build hover text for a proof name span.
mkProofHover :: RelLexicon -> SrcPos -> SrcPos -> CheckedProof -> HoverItem
mkProofHover rl s e cp =
  let inf = checkedInference cp
      cancel = checkedCancellation cp
      nPrems = length (premises inf)
      header =
        "TFL inference ("
          <> showInt nPrems
          <> " premise"
          <> (if nPrems == 1 then "" else "s")
          <> ")"
      algebraic = prettyStatement (conclusion inf)
      english = prettyStatementEnglish rl (conclusion inf)
      conclLine = "Conclusion: " <> algebraic <> "  (" <> english <> ")"
      cancelInfo = prettyCancellation cancel
   in HoverItem s e (header <> "\n" <> conclLine <> "\n\n" <> cancelInfo)

-- | Build hover items for @reference premises.
mkRefHovers :: RelLexicon -> ExternalContext -> [Text] -> Map Text Statement -> [Located Premise] -> [HoverItem]
mkRefHovers rl ext opens ctx = concatMap go
  where
    go lp = case locValue lp of
      PremiseRef mns name mmod ->
        case resolveRef ext opens ctx mns name of
          Just stmt ->
            let base = prettyStatement stmt <> "  (" <> prettyStatementEnglish rl stmt <> ")"
                modified = case mmod of
                  Nothing -> base
                  Just modifier ->
                    let applied = applyRefModifier lp mmod stmt
                     in case applied of
                          Right sh -> case fromConcreteH sh of
                            Just s ->
                              base <> "\n→ " <> prettyStatement s
                                <> "  ("
                                <> prettyStatementEnglish rl s
                                <> ")"
                            Nothing -> base
                          Left _ -> base
             in [HoverItem (locStart lp) (locEnd lp) modified]
          Nothing -> []
      PremiseStmt _ -> []
      PremiseHole _ -> []

-- ---------------------------------------------------------------------------
-- Definition items
-- ---------------------------------------------------------------------------

mkRefDefs :: ExternalContext -> [Text] -> Map Text (SrcPos, SrcPos) -> [Located Premise] -> [DefinitionItem]
mkRefDefs ext opens locs = concatMap go
  where
    go lp = case locValue lp of
      PremiseRef Nothing name _ ->
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
      PremiseStmt _ -> []
      PremiseHole _ -> []

-- | Resolve a reference to a statement (for hover display).
resolveRef :: ExternalContext -> [Text] -> Map Text Statement -> Maybe Text -> Text -> Maybe Statement
resolveRef ext opens ctx Nothing name =
  case Map.lookup name ctx of
    Just stmt -> Just stmt
    Nothing ->
      let hits =
            [ stmt
              | ns <- opens,
                Just entry <- [Map.lookup ns (unExternalContext ext)],
                Just stmt <- [Map.lookup name (nsConclusions entry)]
            ]
       in case hits of
            [stmt] -> Just stmt
            _ -> Nothing
resolveRef ext _ _ (Just ns) name =
  case Map.lookup ns (unExternalContext ext) of
    Just entry -> Map.lookup name (nsConclusions entry)
    Nothing -> Nothing
