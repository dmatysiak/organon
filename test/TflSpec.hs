module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Organon.Tfl.Check
import Organon.Tfl.Document
import Organon.Tfl.Hole
import Organon.Tfl.Parser
import Organon.Tfl.Pretty
import Organon.Tfl.Types
import Organon.Tfl.Validity

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- Shorthand constructors
mkTerm :: Text -> Term
mkTerm n = Term n False

mkCompTerm :: Text -> Term
mkCompTerm n = Term n True

mkST :: WildSign -> Text -> SignedTerm
mkST ws n = SignedTerm ws (Atomic (mkTerm n)) []

mkStmt :: [SignedTerm] -> Statement
mkStmt = Statement

-- Categorical statement builders (algebraic signs)
stmtA :: Text -> Text -> Statement
stmtA s p = mkStmt [mkST (Fixed Minus) s, mkST (Fixed Plus) p]

stmtE :: Text -> Text -> Statement
stmtE s p = mkStmt [mkST (Fixed Minus) s, mkST (Fixed Minus) p]

stmtI :: Text -> Text -> Statement
stmtI s p = mkStmt [mkST (Fixed Plus) s, mkST (Fixed Plus) p]

stmtO :: Text -> Text -> Statement
stmtO s p = mkStmt [mkST (Fixed Plus) s, mkST (Fixed Minus) p]

noErrors :: CheckResult -> [Diagnostic]
noErrors = filter (\d -> diagSeverity d == Error) . checkDiagnostics

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  parserSpec
  validitySpec
  prettySpec
  documentSpec
  checkSpec
  relationalSpec

-- ===================================================================
-- Parser
-- ===================================================================

parserSpec :: Spec
parserSpec = describe "Tfl.Parser" $ do

  describe "Algebraic syntax" $ do
    it "parses a simple A statement: -S +P" $
      parseStatement "-S +P" `shouldBe` Right (stmtA "S" "P")

    it "parses an E statement: -S -P" $
      parseStatement "-S -P" `shouldBe` Right (stmtE "S" "P")

    it "parses an I statement: +S +P" $
      parseStatement "+S +P" `shouldBe` Right (stmtI "S" "P")

    it "parses an O statement: +S -P" $
      parseStatement "+S -P" `shouldBe` Right (stmtO "S" "P")

    it "parses wild sign: *Socrates +P" $
      parseStatement "*Socrates +P"
        `shouldBe` Right (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])

    it "parses complemented term: -non-P +S" $
      parseStatement "-non-P +S"
        `shouldBe` Right
          (mkStmt [SignedTerm (Fixed Minus) (Atomic (mkCompTerm "P")) [], mkST (Fixed Plus) "S"])

    it "parses three-term sorites statement: -S +M -P" $
      parseStatement "-S +M -P"
        `shouldBe` Right
          (mkStmt [mkST (Fixed Minus) "S", mkST (Fixed Plus) "M", mkST (Fixed Minus) "P"])

    it "parses spaced signs: - S + P" $
      parseStatement "- S + P" `shouldBe` Right (stmtA "S" "P")

    it "parses mixed spacing: -S + P" $
      parseStatement "-S + P" `shouldBe` Right (stmtA "S" "P")

    it "parses spaced wild: * Socrates + P" $
      parseStatement "* Socrates + P"
        `shouldBe` Right (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])

    it "parses spaced inference: - M + P ; - S + M ; ∴ - S + P" $
      parseInference "- M + P ; - S + M ; ∴ - S + P"
        `shouldBe` Right
          Inference
            { premises = [stmtA "M" "P", stmtA "S" "M"],
              conclusion = stmtA "S" "P"
            }

    it "parses a two-premise algebraic inference" $
      parseInference "-M +P\n-S +M\n∴ -S +P"
        `shouldBe` Right
          Inference
            { premises = [stmtA "M" "P", stmtA "S" "M"],
              conclusion = stmtA "S" "P"
            }

    it "parses inference with semicolons" $
      parseInference "-M +P ; -S +M ; ∴ -S +P"
        `shouldBe` Right
          Inference
            { premises = [stmtA "M" "P", stmtA "S" "M"],
              conclusion = stmtA "S" "P"
            }

  describe "Positional subscripts" $ do
    it "parses single subscript: -Boy<1>" $
      parseStatement "-Boy<1> +P"
        `shouldBe` Right
          (mkStmt [SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1], mkST (Fixed Plus) "P"])

    it "parses multi subscript: +Lover<1,2>" $
      parseStatement "+Lover<1,2>"
        `shouldBe` Right
          (mkStmt [SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2]])

    it "parses full relational statement: -Boy<1> +Lover<1,2> +Girl<2>" $
      parseStatement "-Boy<1> +Lover<1,2> +Girl<2>"
        `shouldBe` Right
          (mkStmt
            [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
              SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2],
              SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]
            ])

    it "parses wild with subscript: *Socrates<1>" $
      parseStatement "*Socrates<1> +Lover<1,2> -Human<2>"
        `shouldBe` Right
          (mkStmt
            [ SignedTerm Wild (Atomic (mkTerm "Socrates")) [1],
              SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2],
              SignedTerm (Fixed Minus) (Atomic (mkTerm "Human")) [2]
            ])

    it "parses complemented with subscript: -non-Boy<1>" $
      parseStatement "-non-Boy<1> +Lover<1,2>"
        `shouldBe` Right
          (mkStmt
            [ SignedTerm (Fixed Minus) (Atomic (mkCompTerm "Boy")) [1],
              SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2]
            ])

    it "no subscript means monadic (backward compat)" $
      parseStatement "-S +P"
        `shouldBe` Right (stmtA "S" "P")

    it "parses relational inference" $
      parseInference "-Boy<1> +Lover<1,2> +Girl<2>\n-Girl<2> +Reader<2,3> +Book<3>\n∴ -Boy<1> +Lover<1,2> +Reader<2,3> +Book<3>"
        `shouldBe` Right
          Inference
            { premises =
                [ mkStmt
                    [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                      SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2],
                      SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]
                    ],
                  mkStmt
                    [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Girl")) [2],
                      SignedTerm (Fixed Plus) (Atomic (mkTerm "Reader")) [2, 3],
                      SignedTerm (Fixed Plus) (Atomic (mkTerm "Book")) [3]
                    ]
                ],
              conclusion =
                mkStmt
                  [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                    SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2],
                    SignedTerm (Fixed Plus) (Atomic (mkTerm "Reader")) [2, 3],
                    SignedTerm (Fixed Plus) (Atomic (mkTerm "Book")) [3]
                  ]
            }

  describe "English syntax" $ do
    it "parses 'every S is P'" $
      parseStatement "every S is P" `shouldBe` Right (stmtA "S" "P")

    it "parses 'no S is P'" $
      parseStatement "no S is P" `shouldBe` Right (stmtE "S" "P")

    it "parses 'some S is P'" $
      parseStatement "some S is P" `shouldBe` Right (stmtI "S" "P")

    it "parses 'some S is not P'" $
      parseStatement "some S is not P" `shouldBe` Right (stmtO "S" "P")

    it "parses wild English: '* Socrates is P'" $
      parseStatement "* Socrates is P"
        `shouldBe` Right (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])

    it "parses 'every non-S is P'" $
      parseStatement "every non-S is P"
        `shouldBe` Right
          (mkStmt [SignedTerm (Fixed Minus) (Atomic (mkCompTerm "S")) [], mkST (Fixed Plus) "P"])

    it "parses English inference" $
      parseInference "every M is P\nevery S is M\n∴ every S is P"
        `shouldBe` Right
          Inference
            { premises = [stmtA "M" "P", stmtA "S" "M"],
              conclusion = stmtA "S" "P"
            }

  describe "Round-trips" $ do
    it "algebraic parse → pretty is correct" $ do
      let input = "-S +P"
      case parseStatement input of
        Right stmt -> prettyStatement stmt `shouldBe` "−S +P"
        Left err -> expectationFailure (T.unpack err)

    it "English parse → English pretty → parse round-trips" $ do
      let input = "some S is not P"
      case parseStatement input of
        Right stmt -> do
          let rendered = prettyStatementEnglish Map.empty stmt
          rendered `shouldBe` "some S is not P"
          parseStatement rendered `shouldBe` Right stmt
        Left err -> expectationFailure (T.unpack err)

    it "algebraic inference pretty output" $ do
      let input = "-M +P\n-S +M\n∴ -S +P"
      case parseInference input of
        Right inf -> prettyInference inf `shouldBe` "−M +P\n−S +M\n∴ −S +P"
        Left err -> expectationFailure (T.unpack err)

  describe "Hole parsing" $ do
    it "parses a whole-statement hole" $
      parseStatementH "?" `shouldBe` Right WholeStmtH

    it "parses algebraic statement with term hole" $
      parseStatementH "-S ?"
        `shouldBe` Right (StmtH [ConcreteSTH (mkST (Fixed Minus) "S"), HoleSTH])

    it "parses English statement as concrete in hole mode" $
      parseStatementH "every S is P"
        `shouldBe` Right
          (StmtH [ConcreteSTH (mkST (Fixed Minus) "S"), ConcreteSTH (mkST (Fixed Plus) "P")])

  describe "Error cases" $ do
    it "rejects empty input" $
      parseStatement "" `shouldSatisfy` isLeft'

    it "rejects bare term without sign" $
      -- Only when algebraic (no quantifier) — bare name isn't valid
      parseStatement "S" `shouldSatisfy` isLeft'

-- ===================================================================
-- Validity
-- ===================================================================

validitySpec :: Spec
validitySpec = describe "Tfl.Validity" $ do

  describe "Two-premise syllogisms" $ do
    it "validates Barbara: -M +P, -S +M ∴ -S +P" $ do
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtA "S" "P")
      validate inf `shouldSatisfy` isValid

    it "validates Celarent: -M -P, -S +M ∴ -S -P" $ do
      let inf = Inference [stmtE "M" "P", stmtA "S" "M"] (stmtE "S" "P")
      validate inf `shouldSatisfy` isValid

    it "validates Darii: -M +P, +S +M ∴ +S +P" $ do
      let inf = Inference [stmtA "M" "P", stmtI "S" "M"] (stmtI "S" "P")
      validate inf `shouldSatisfy` isValid

    it "validates Ferio: -M -P, +S +M ∴ +S -P" $ do
      let inf = Inference [stmtE "M" "P", stmtI "S" "M"] (stmtO "S" "P")
      validate inf `shouldSatisfy` isValid

    it "validates Cesare (fig 2): -P -M, -S +M ∴ -S -P" $ do
      let inf = Inference [stmtE "P" "M", stmtA "S" "M"] (stmtE "S" "P")
      validate inf `shouldSatisfy` isValid

    it "validates Camestres (fig 2): -P +M, -S -M ∴ -S -P" $ do
      let inf = Inference [stmtA "P" "M", stmtE "S" "M"] (stmtE "S" "P")
      validate inf `shouldSatisfy` isValid

  describe "Sorites" $ do
    it "validates a 3-premise sorites" $ do
      -- -A +B, -B +C, -C +D ∴ -A +D
      let inf =
            Inference
              [stmtA "A" "B", stmtA "B" "C", stmtA "C" "D"]
              (stmtA "A" "D")
      validate inf `shouldSatisfy` isValid

    it "validates a 4-premise sorites" $ do
      -- -A +B, -B +C, -C +D, -D +E ∴ -A +E
      let inf =
            Inference
              [stmtA "A" "B", stmtA "B" "C", stmtA "C" "D", stmtA "D" "E"]
              (stmtA "A" "E")
      validate inf `shouldSatisfy` isValid

  describe "Wild quantity" $ do
    it "wild cancels with plus: *Socrates +M, -M +P ∴ *Socrates +P" $ do
      let inf =
            Inference
              [ mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "M"],
                stmtA "M" "P"
              ]
              (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])
      validate inf `shouldSatisfy` isValid

    it "wild cancels with minus" $ do
      let inf =
            Inference
              [ mkStmt [mkST Wild "Socrates", mkST (Fixed Minus) "M"],
                mkStmt [mkST (Fixed Plus) "M", mkST (Fixed Plus) "P"]
              ]
              (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])
      validate inf `shouldSatisfy` isValid

  describe "Invalid inferences" $ do
    it "detects undistributed middle: -M +P, -S +M ∴ -S -P" $ do
      -- Correct conclusion would be -S +P, not -S -P
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtE "S" "P")
      validate inf `shouldSatisfy` isInvalid

    it "detects wrong conclusion term" $ do
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtA "S" "Q")
      validate inf `shouldSatisfy` isInvalid

    it "detects extra conclusion term" $ do
      let inf =
            Inference
              [stmtA "M" "P"]
              (mkStmt [mkST (Fixed Minus) "S", mkST (Fixed Plus) "P", mkST (Fixed Plus) "M"])
      validate inf `shouldSatisfy` isInvalid

  describe "Cancellation details" $ do
    it "cancellation of Barbara has one cancelled pair (M)" $ do
      let cancel = cancellation [stmtA "M" "P", stmtA "S" "M"]
      length (cancelled cancel) `shouldBe` 1
      let cp = head (cancelled cancel)
      cancelledTermExpr cp `shouldBe` Atomic (mkTerm "M")
      cancelledPositions cp `shouldBe` []

    it "uncancelled terms of Barbara are -S and +P" $ do
      let cancel = cancellation [stmtA "M" "P", stmtA "S" "M"]
      map snd (uncancelled cancel)
        `shouldMatchList` [mkST (Fixed Minus) "S", mkST (Fixed Plus) "P"]

  describe "Relational cancellation" $ do
    it "cancels Girl<2> across relational premises" $ do
      -- -Boy<1> +Lover<1,2> +Girl<2>  and  -Girl<2> +Reader<2,3> +Book<3>
      let p1 = mkStmt
                 [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Lover")) [1, 2],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Girl")) [2]
                 ]
          p2 = mkStmt
                 [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Girl")) [2],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Reader")) [2, 3],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Book")) [3]
                 ]
          cancel = cancellation [p1, p2]
      length (cancelled cancel) `shouldBe` 1
      cancelledTermExpr (head (cancelled cancel)) `shouldBe` Atomic (mkTerm "Girl")
      cancelledPositions (head (cancelled cancel)) `shouldBe` [2]
      map snd (uncancelled cancel)
        `shouldMatchList`
          [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
            SignedTerm (Fixed Plus)  (Atomic (mkTerm "Lover")) [1, 2],
            SignedTerm (Fixed Plus)  (Atomic (mkTerm "Reader")) [2, 3],
            SignedTerm (Fixed Plus)  (Atomic (mkTerm "Book")) [3]
          ]

    it "validates relational inference: boy-lover-girl chained with girl-reader-book" $ do
      let p1 = mkStmt
                 [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Lover")) [1, 2],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Girl")) [2]
                 ]
          p2 = mkStmt
                 [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Girl")) [2],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Reader")) [2, 3],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Book")) [3]
                 ]
          concl = mkStmt
                    [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                      SignedTerm (Fixed Plus)  (Atomic (mkTerm "Lover")) [1, 2],
                      SignedTerm (Fixed Plus)  (Atomic (mkTerm "Reader")) [2, 3],
                      SignedTerm (Fixed Plus)  (Atomic (mkTerm "Book")) [3]
                    ]
          inf = Inference [p1, p2] concl
      validate inf `shouldSatisfy` isValid

    it "does not cancel terms at different positions" $ do
      -- +Girl<2> should NOT cancel with -Girl<5>
      let p1 = mkStmt [SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]]
          p2 = mkStmt [SignedTerm (Fixed Minus) (Atomic (mkTerm "Girl")) [5]]
          cancel = cancellation [p1, p2]
      length (cancelled cancel) `shouldBe` 0
      length (uncancelled cancel) `shouldBe` 2

    it "does not cancel subscripted term with unsubscripted" $ do
      -- +Girl<2> should NOT cancel with -Girl (monadic)
      let p1 = mkStmt [SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]]
          p2 = mkStmt [mkST (Fixed Minus) "Girl"]
          cancel = cancellation [p1, p2]
      length (cancelled cancel) `shouldBe` 0

    it "wild cancels in relational context" $ do
      -- *Socrates<1> +Lover<1,2>  and  -Lover<1,2> +P
      let p1 = mkStmt
                 [ SignedTerm Wild (Atomic (mkTerm "Socrates")) [1],
                   SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2]
                 ]
          p2 = mkStmt
                 [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Lover")) [1, 2],
                   SignedTerm (Fixed Plus) (Atomic (mkTerm "P")) []
                 ]
          cancel = cancellation [p1, p2]
      length (cancelled cancel) `shouldBe` 1
      cancelledTermExpr (head (cancelled cancel)) `shouldBe` Atomic (mkTerm "Lover")
      cancelledPositions (head (cancelled cancel)) `shouldBe` [1, 2]

    it "rejects relational inference with wrong conclusion positions" $ do
      let p1 = mkStmt
                 [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                   SignedTerm (Fixed Plus)  (Atomic (mkTerm "Girl")) [2]
                 ]
          -- wrong: conclusion says Girl<3> instead of Girl<2>
          concl = mkStmt
                    [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
                      SignedTerm (Fixed Plus)  (Atomic (mkTerm "Girl")) [3]
                    ]
          inf = Inference [p1] concl
      validate inf `shouldSatisfy` isInvalid

-- ===================================================================
-- Pretty printer
-- ===================================================================

prettySpec :: Spec
prettySpec = describe "Tfl.Pretty" $ do

  describe "Algebraic rendering" $ do
    it "renders A: −S +P" $
      prettyStatement (stmtA "S" "P") `shouldBe` "−S +P"

    it "renders E: −S −P" $
      prettyStatement (stmtE "S" "P") `shouldBe` "−S −P"

    it "renders I: +S +P" $
      prettyStatement (stmtI "S" "P") `shouldBe` "+S +P"

    it "renders O: +S −P" $
      prettyStatement (stmtO "S" "P") `shouldBe` "+S −P"

    it "renders complemented: −non-S +P" $
      prettyStatement (mkStmt [SignedTerm (Fixed Minus) (Atomic (mkCompTerm "S")) [], mkST (Fixed Plus) "P"])
        `shouldBe` "−non-S +P"

    it "renders wild: *Socrates +P" $
      prettyStatement (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])
        `shouldBe` "*Socrates +P"

  describe "English rendering" $ do
    it "renders A: every S is P" $
      prettyStatementEnglish Map.empty (stmtA "S" "P") `shouldBe` "every S is P"

    it "renders E: no S is P" $
      prettyStatementEnglish Map.empty (stmtE "S" "P") `shouldBe` "no S is P"

    it "renders I: some S is P" $
      prettyStatementEnglish Map.empty (stmtI "S" "P") `shouldBe` "some S is P"

    it "renders O: some S is not P" $
      prettyStatementEnglish Map.empty (stmtO "S" "P") `shouldBe` "some S is not P"

    it "renders wild English: * Socrates is P" $
      prettyStatementEnglish Map.empty (mkStmt [mkST Wild "Socrates", mkST (Fixed Plus) "P"])
        `shouldBe` "* Socrates is P"

    it "falls back to algebraic for 3-term statement" $
      prettyStatementEnglish Map.empty (mkStmt [mkST (Fixed Minus) "S", mkST (Fixed Plus) "M", mkST (Fixed Minus) "P"])
        `shouldBe` "−S +M −P"

  describe "Positional subscript rendering" $ do
    it "renders single subscript: −Boy<1>" $
      prettySignedTerm (SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1])
        `shouldBe` "−Boy<1>"

    it "renders multi subscript: +Lover<1,2>" $
      prettySignedTerm (SignedTerm (Fixed Plus) (Atomic (mkTerm "Lover")) [1, 2])
        `shouldBe` "+Lover<1,2>"

    it "renders monadic term without subscript" $
      prettySignedTerm (mkST (Fixed Plus) "P") `shouldBe` "+P"

    it "renders full relational statement" $
      prettyStatement
        (mkStmt
          [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1],
            SignedTerm (Fixed Plus)  (Atomic (mkTerm "Lover")) [1, 2],
            SignedTerm (Fixed Plus)  (Atomic (mkTerm "Girl")) [2]
          ])
        `shouldBe` "−Boy<1> +Lover<1,2> +Girl<2>"

    it "round-trips relational statement through parse → pretty" $ do
      let input = "-Boy<1> +Lover<1,2> +Girl<2>"
      case parseStatement input of
        Right stmt -> prettyStatement stmt `shouldBe` "−Boy<1> +Lover<1,2> +Girl<2>"
        Left err -> expectationFailure (T.unpack err)

  describe "Inference rendering" $ do
    it "renders algebraic inference" $ do
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtA "S" "P")
      prettyInference inf `shouldBe` "−M +P\n−S +M\n∴ −S +P"

    it "renders English inference" $ do
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtA "S" "P")
      prettyInferenceEnglish Map.empty inf `shouldBe` "every M is P\nevery S is M\n∴ every S is P"

  describe "Validation result rendering" $ do
    it "renders Valid with cancellation" $ do
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtA "S" "P")
      case validate inf of
        Valid cancel -> do
          let txt = prettyValidationResult (Valid cancel)
          txt `shouldSatisfy` T.isPrefixOf "Valid"
          txt `shouldSatisfy` T.isInfixOf "Cancelled"
        Invalid _ _ -> expectationFailure "Expected Valid"

    it "renders Invalid with errors" $ do
      let inf = Inference [stmtA "M" "P", stmtA "S" "M"] (stmtE "S" "P")
      case validate inf of
        Invalid cancel errs -> do
          let txt = prettyValidationResult (Invalid cancel errs)
          txt `shouldSatisfy` T.isPrefixOf "Invalid"
          txt `shouldSatisfy` T.isInfixOf "Errors"
        Valid _ -> expectationFailure "Expected Invalid"

-- ===================================================================
-- Document
-- ===================================================================

documentSpec :: Spec
documentSpec = describe "Tfl.Document" $ do

  describe "Proof block parsing" $ do
    it "parses a minimal proof block" $
      case parseDocument "proof Test\n-M +P\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          length (docProofs doc) `shouldBe` 1
          let block = locValue (head (docProofs doc))
          locValue (proofName block) `shouldBe` "Test"
          length (proofPremises block) `shouldBe` 2
        Left err -> expectationFailure (T.unpack err)

    it "parses English proof block" $
      case parseDocument "proof Barbara\nevery M is P\nevery S is M\n∴ every S is P\n" of
        Right doc -> do
          length (docProofs doc) `shouldBe` 1
          let block = locValue (head (docProofs doc))
          locValue (proofName block) `shouldBe` "Barbara"
        Left err -> expectationFailure (T.unpack err)

    it "parses multiple proof blocks" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n-N -P\n-S +N\n∴ -S -P\n" of
        Right doc -> length (docProofs doc) `shouldBe` 2
        Left err -> expectationFailure (T.unpack err)

  describe "References" $ do
    it "parses @ref premise" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let block = locValue (docProofs doc !! 1)
          case locValue (head (proofPremises block)) of
            PremiseRef Nothing name Nothing -> name `shouldBe` "A"
            other -> expectationFailure ("Expected PremiseRef, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

    it "parses qualified @Ns.Name" $
      case parseDocument "proof A\n@Basics.Test\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          case locValue (head (proofPremises block)) of
            PremiseRef (Just ns) name Nothing -> do
              ns `shouldBe` "Basics"
              name `shouldBe` "Test"
            other -> expectationFailure ("Expected qualified PremiseRef, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

    it "parses @ref conv" $
      case parseDocument "proof A\n-M -P\n-S +M\n∴ -S -P\n\nproof B\n@A conv\n-S +M\n∴ -S -P\n" of
        Right doc -> do
          let block = locValue (docProofs doc !! 1)
          case locValue (head (proofPremises block)) of
            PremiseRef Nothing name (Just RefConv) -> name `shouldBe` "A"
            other -> expectationFailure ("Expected RefConv, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

    it "parses @ref per-accidens" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A per-accidens\n+S +M\n∴ +S +P\n" of
        Right doc -> do
          let block = locValue (docProofs doc !! 1)
          case locValue (head (proofPremises block)) of
            PremiseRef Nothing name (Just RefPerAccidens) -> name `shouldBe` "A"
            other -> expectationFailure ("Expected RefPerAccidens, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

    it "parses @ref obv" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A obv\n-S +M\n∴ -S -P\n" of
        Right doc -> do
          let block = locValue (docProofs doc !! 1)
          case locValue (head (proofPremises block)) of
            PremiseRef Nothing name (Just RefObv) -> name `shouldBe` "A"
            other -> expectationFailure ("Expected RefObv, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

    it "parses @ref contra" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A contra\n+S -M\n∴ +S +P\n" of
        Right doc -> do
          let block = locValue (docProofs doc !! 1)
          case locValue (head (proofPremises block)) of
            PremiseRef Nothing name (Just RefContra) -> name `shouldBe` "A"
            other -> expectationFailure ("Expected RefContra, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

  describe "Open directives" $ do
    it "parses open directive" $
      case parseDocument "open Basics\n\nproof A\n-M +P\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          length (docOpens doc) `shouldBe` 1
          locValue (head (docOpens doc)) `shouldBe` "Basics"
        Left err -> expectationFailure (T.unpack err)

  describe "Holes in document" $ do
    it "parses conclusion hole" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ ?\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          locValue (proofConclusion block) `shouldBe` WholeStmtH
        Left err -> expectationFailure (T.unpack err)

    it "parses premise hole" $
      case parseDocument "proof A\n?\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          case locValue (head (proofPremises block)) of
            PremiseHole WholeStmtH -> pure ()
            other -> expectationFailure ("Expected PremiseHole, got: " <> show other)
        Left err -> expectationFailure (T.unpack err)

  describe "fromConcreteH" $ do
    it "extracts concrete statement" $
      fromConcreteH (StmtH [ConcreteSTH (mkST (Fixed Minus) "S"), ConcreteSTH (mkST (Fixed Plus) "P")])
        `shouldBe` Just (stmtA "S" "P")

    it "returns Nothing for statement with hole" $
      fromConcreteH (StmtH [ConcreteSTH (mkST (Fixed Minus) "S"), HoleSTH])
        `shouldBe` Nothing

    it "returns Nothing for WholeStmtH" $
      fromConcreteH WholeStmtH `shouldBe` Nothing

-- ===================================================================
-- Checker
-- ===================================================================

checkSpec :: Spec
checkSpec = describe "Tfl.Check" $ do

  describe "Valid proofs" $ do
    it "checks Barbara" $
      case parseDocument "proof Barbara\n-M +P\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
          length (checkProofs result) `shouldBe` 1
        Left err -> expectationFailure (T.unpack err)

    it "checks Celarent" $
      case parseDocument "proof Celarent\n-M -P\n-S +M\n∴ -S -P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

    it "checks multiple proofs" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n-N -P\n-S +N\n∴ -S -P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
          length (checkProofs result) `shouldBe` 2
        Left err -> expectationFailure (T.unpack err)

  describe "Invalid proofs" $ do
    it "reports error for invalid inference" $
      case parseDocument "proof Bad\n-M +P\n-S +M\n∴ -S -P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldSatisfy` (not . null)
        Left err -> expectationFailure (T.unpack err)

  describe "Reference resolution" $ do
    it "resolves local @ref" $
      -- A proves -S +P. B uses @A as premise (-S +P) with -P +Q. Concl: -S +Q.
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A\n-P +Q\n∴ -S +Q\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
          length (checkProofs result) `shouldBe` 2
        Left err -> expectationFailure (T.unpack err)

    it "reports unknown @ref" $
      case parseDocument "proof B\n@Unknown\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldSatisfy` any (\d -> "Unknown reference" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "resolves @ref from external namespace" $ do
      let ext =
            ExternalContext $
              Map.singleton
                "Ext"
                NamespaceEntry
                  { nsFilePath = "ext.tfl",
                    nsConclusions = Map.singleton "Thm" (stmtA "S" "P"),
                    nsLocations = Map.singleton "Thm" (SrcPos 1 1, SrcPos 1 10)
                  }
      -- @Thm resolves to -S +P. Second premise: -P +Q. Concl: -S +Q.
      case parseDocument "open Ext\n\nproof B\n@Thm\n-P +Q\n∴ -S +Q\n" of
        Right doc -> do
          let result = checkDocument ext doc
          noErrors result `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

  describe "Reference modifiers" $ do
    it "resolves @ref conv on E statement" $
      -- A proves -S -P (E). conv(E) = -P -S.
      -- B: @A conv gives -P -S. Then -Q +P. ∴ -Q -S.
      case parseDocument "proof A\n-M -P\n-S +M\n∴ -S -P\n\nproof B\n@A conv\n-Q +S\n∴ -Q -P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

    it "resolves @ref per-accidens on A statement" $
      -- A proves -S +P (A). per-accidens(A) = +P +S (I).
      -- B: @A per-accidens gives +P +S. Then -P +M. ∴ +M +S? No...
      -- Simpler: per-accidens(A -S +P) = +P +S
      -- B: +P +S is a premise. Second premise -P +Q. ∴ +Q +S? ... Let's keep it simple.
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A per-accidens\n-P +Q\n∴ +S +Q\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          -- per-accidens(-S +P) = +P +S. Premises: +P +S, -P +Q.
          -- Cancel P: +P vs -P. Uncancelled: +S, +Q. Concl: +S +Q. ✓
          noErrors result `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

    it "resolves @ref obv" $
      -- A proves -S +P. obv(-S +P) = -S -non-P.
      -- B: @A obv gives -S -non-P. Second premise: +non-P +Q (cancel non-P). ∴ -S +Q? No.
      -- Actually: obv(-S +P) = -S -non-P. Terms: -S, -non-P.
      -- Second premise needs +non-P to cancel. Use: +non-P +Q.
      -- Uncancelled: -S (prem0) and +Q (prem1). Concl: +Q -S? No, -S +Q.
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A obv\n+non-P +Q\n∴ -S +Q\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

    it "resolves @ref contra on A statement" $
      -- A proves -S +P (A). contra: SignedTerm (sign +P) (non-P), SignedTerm (sign -S) (non-S)
      -- = +non-P -non-S.
      -- B: @A contra gives +non-P -non-S. Second premise: +non-S +Q (cancel non-S).
      -- Uncancelled: +non-P (prem0), +Q (prem1). Concl: +non-P +Q.
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A contra\n+non-S +Q\n∴ +non-P +Q\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

    it "rejects conv on A statement" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A conv\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldSatisfy`
            any (\d -> "Cannot apply simple conversion" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "rejects contra on E statement" $
      case parseDocument "proof A\n-M -P\n-S +M\n∴ -S -P\n\nproof B\n@A contra\n-S +M\n∴ -S -P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          noErrors result `shouldSatisfy`
            any (\d -> "Cannot apply contraposition" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

  describe "Duplicate names" $ do
    it "reports duplicate proof name" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof A\n-M -P\n-S +M\n∴ -S -P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy`
            any (\d -> "Duplicate proof name" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

  describe "Hovers" $ do
    it "produces hover for proof name" $
      case parseDocument "proof Barbara\n-M +P\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHovers result `shouldSatisfy` (not . null)
          checkHovers result `shouldSatisfy`
            any (\h -> "TFL inference" `T.isInfixOf` hoverText h)
        Left err -> expectationFailure (T.unpack err)

  describe "Hole solving" $ do
    it "solves conclusion hole from concrete premises" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ ?\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHoleFills result `shouldSatisfy` (not . null)
        Left err -> expectationFailure (T.unpack err)

    it "warns when premises have holes" $
      case parseDocument "proof A\n?\n-S +M\n∴ -S +P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy`
            any (\d -> "holes" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

  describe "Definition items" $ do
    it "produces definition for @ref" $
      case parseDocument "proof A\n-M +P\n-S +M\n∴ -S +P\n\nproof B\n@A\n+S +M\n∴ +S +P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDefinitions result `shouldSatisfy` (not . null)
        Left err -> expectationFailure (T.unpack err)

-- ===================================================================
-- Relational terms (Phase 12)
-- ===================================================================

relationalSpec :: Spec
relationalSpec = describe "Relational terms" $ do
  let loveLexicon = Map.fromList [("Love", RelForms "Lover-of" "Loved-by")]

  describe "Pretty-printer" $ do
    it "renders active relational English" $ do
      let stmt = Statement
            [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1]
            , SignedTerm (Fixed Plus) (Atomic (mkTerm "Love")) [1, 2]
            , SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]
            ]
      prettyStatementEnglish loveLexicon stmt
        `shouldBe` "every Boy is Lover-of some Girl"

    it "renders passive relational English" $ do
      -- Girl has position [2], which does not match relation's first
      -- position (1), so the passive form is used.
      let stmt = Statement
            [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Girl")) [2]
            , SignedTerm (Fixed Plus) (Atomic (mkTerm "Love")) [1, 2]
            , SignedTerm (Fixed Plus) (Atomic (mkTerm "Boy")) [1]
            ]
      prettyStatementEnglish loveLexicon stmt
        `shouldBe` "every Girl is Loved-by some Boy"

    it "uses default forms when lexicon is empty" $ do
      let stmt = Statement
            [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1]
            , SignedTerm (Fixed Plus) (Atomic (mkTerm "Love")) [1, 2]
            , SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]
            ]
      prettyStatementEnglish Map.empty stmt
        `shouldBe` "every Boy is Love-of some Girl"

    it "renders relational inference English" $ do
      let inf = Inference
            [ Statement
                [ SignedTerm (Fixed Minus) (Atomic (mkTerm "Boy")) [1]
                , SignedTerm (Fixed Plus) (Atomic (mkTerm "Love")) [1, 2]
                , SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]
                ]
            ]
            ( Statement
                [ SignedTerm (Fixed Plus) (Atomic (mkTerm "Boy")) [1]
                , SignedTerm (Fixed Plus) (Atomic (mkTerm "Love")) [1, 2]
                , SignedTerm (Fixed Plus) (Atomic (mkTerm "Girl")) [2]
                ]
            )
      prettyInferenceEnglish loveLexicon inf
        `shouldBe` "every Boy is Lover-of some Girl\n∴ some Boy is Lover-of some Girl"

  describe "English parser" $ do
    it "parses active relational: every Boy is Love-of some Girl" $
      case parseStatement "every Boy is Love-of some Girl" of
        Right (Statement sts) -> do
          length sts `shouldBe` 3
          positions (sts !! 0) `shouldBe` [1]
          positions (sts !! 1) `shouldBe` [1, 2]
          positions (sts !! 2) `shouldBe` [2]
          termName (term (sts !! 1)) `shouldBe` "Love"
        Left err -> expectationFailure (T.unpack err)

    it "parses passive relational: some Girl is Love-by every Boy" $
      case parseStatement "some Girl is Love-by every Boy" of
        Right (Statement sts) -> do
          length sts `shouldBe` 3
          positions (sts !! 0) `shouldBe` [2]  -- passive: subj gets pos 2
          positions (sts !! 1) `shouldBe` [1, 2]
          positions (sts !! 2) `shouldBe` [1]  -- passive: obj gets pos 1
          termName (term (sts !! 1)) `shouldBe` "Love"
        Left err -> expectationFailure (T.unpack err)

    it "falls back to monadic for non-relational terms" $
      case parseStatement "every Man is Mortal" of
        Right (Statement sts) -> do
          length sts `shouldBe` 2
          positions (sts !! 0) `shouldBe` []
          positions (sts !! 1) `shouldBe` []
        Left err -> expectationFailure (T.unpack err)

  describe "Document rel directive" $ do
    it "parses rel declaration" $
      case parseDocument "rel Love \"Lover of\" \"Loved by\"\n\nproof A\n-Boy<1> +Love<1,2> +Girl<2>\n∴ +Boy<1> +Love<1,2> +Girl<2>\n" of
        Right doc -> do
          let rl = docRelLexicon doc
          Map.member "Love" rl `shouldBe` True
          relActive (rl Map.! "Love") `shouldBe` "Lover of"
          relPassive (rl Map.! "Love") `shouldBe` "Loved by"
        Left err -> expectationFailure (T.unpack err)

  describe "Lexicon defaults" $ do
    it "defaultRelForms appends -of and -by" $ do
      let forms = defaultRelForms "Hate"
      relActive forms `shouldBe` "Hate-of"
      relPassive forms `shouldBe` "Hate-by"

    it "lookupRelForms uses lexicon when present" $ do
      let forms = lookupRelForms loveLexicon "Love"
      relActive forms `shouldBe` "Lover-of"
      relPassive forms `shouldBe` "Loved-by"

    it "lookupRelForms falls back to defaults" $ do
      let forms = lookupRelForms loveLexicon "Hate"
      relActive forms `shouldBe` "Hate-of"
      relPassive forms `shouldBe` "Hate-by"

-- ===================================================================
-- Compound terms (Phase 13)
-- ===================================================================

  describe "Compound term parser (algebraic)" $ do
    it "parses +(A + B)" $ do
      case parseStatement "-S +(Farmer + Gentleman)" of
        Right (Statement [_, st]) -> do
          sign st `shouldBe` Fixed Plus
          termExpr st `shouldBe` Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "Farmer")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "Gentleman")) []]
        Right _ -> expectationFailure "expected 2 terms"
        Left err -> expectationFailure (T.unpack err)

    it "parses -(non-A + non-B)" $ do
      case parseStatement "-S -(non-Farmer + non-Gentleman)" of
        Right (Statement [_, st]) -> do
          sign st `shouldBe` Fixed Minus
          termExpr st `shouldBe` Compound [SignedTerm (Fixed Plus) (Atomic (mkCompTerm "Farmer")) [], SignedTerm (Fixed Plus) (Atomic (mkCompTerm "Gentleman")) []]
        Right _ -> expectationFailure "expected 2 terms"
        Left err -> expectationFailure (T.unpack err)

    it "parses nested +(A + (B + C))" $ do
      case parseStatement "-S +(A + (B + C))" of
        Right (Statement [_, st]) -> do
          sign st `shouldBe` Fixed Plus
          let inner = Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "B")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "C")) []]
          termExpr st `shouldBe` Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "A")) [], SignedTerm (Fixed Plus) inner []]
        Right _ -> expectationFailure "expected 2 terms"
        Left err -> expectationFailure (T.unpack err)

  describe "Compound term parser (English)" $ do
    it "parses 'every Senator is Farmer and Gentleman'" $ do
      case parseStatement "every Senator is Farmer and Gentleman" of
        Right (Statement [subj, pred']) -> do
          sign subj `shouldBe` Fixed Minus
          termExpr subj `shouldBe` Atomic (mkTerm "Senator")
          sign pred' `shouldBe` Fixed Plus
          termExpr pred' `shouldBe` Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "Farmer")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "Gentleman")) []]
        Right _ -> expectationFailure "expected 2 terms"
        Left err -> expectationFailure (T.unpack err)

    it "parses 'some X is not (both non-A and non-B)'" $ do
      case parseStatement "some X is not (both non-A and non-B)" of
        Right (Statement [subj, pred']) -> do
          sign subj `shouldBe` Fixed Plus
          sign pred' `shouldBe` Fixed Minus
          termExpr pred' `shouldBe` Compound [SignedTerm (Fixed Plus) (Atomic (mkCompTerm "A")) [], SignedTerm (Fixed Plus) (Atomic (mkCompTerm "B")) []]
        Right _ -> expectationFailure "expected 2 terms"
        Left err -> expectationFailure (T.unpack err)

  describe "Compound term pretty printer" $ do
    it "renders +(Farmer + Gentleman) algebraically" $ do
      let st = SignedTerm (Fixed Plus) (Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "Farmer")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "Gentleman")) []]) []
      prettySignedTerm st `shouldBe` "+(Farmer + Gentleman)"

    it "renders compound English: every S is Farmer and Gentleman" $ do
      let stmt = Statement [mkST (Fixed Minus) "Senator", SignedTerm (Fixed Plus) (Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "Farmer")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "Gentleman")) []]) []]
      prettyStatementEnglish Map.empty stmt `shouldBe` "every Senator is Farmer and Gentleman"

    it "renders negated compound English: is not (both non-A and non-B)" $ do
      let stmt = Statement [mkST (Fixed Minus) "S", SignedTerm (Fixed Minus) (Compound [SignedTerm (Fixed Plus) (Atomic (mkCompTerm "A")) [], SignedTerm (Fixed Plus) (Atomic (mkCompTerm "B")) []]) []]
      prettyStatementEnglish Map.empty stmt `shouldBe` "every S is not (both non-A and non-B)"

  describe "Compound term validity" $ do
    it "compound cancels as a unit" $ do
      let compound = Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "A")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "B")) []]
          p1 = mkStmt [mkST (Fixed Minus) "S", SignedTerm (Fixed Plus) compound []]
          p2 = mkStmt [SignedTerm (Fixed Minus) compound [], mkST (Fixed Plus) "P"]
          concl = mkStmt [mkST (Fixed Minus) "S", mkST (Fixed Plus) "P"]
          inf = Inference [p1, p2] concl
      validate inf `shouldSatisfy` isValid

    it "compound does not cancel with individual atomic terms" $ do
      let compound = Compound [SignedTerm (Fixed Plus) (Atomic (mkTerm "A")) [], SignedTerm (Fixed Plus) (Atomic (mkTerm "B")) []]
          p1 = mkStmt [mkST (Fixed Minus) "S", SignedTerm (Fixed Plus) compound []]
          p2 = mkStmt [mkST (Fixed Minus) "A", mkST (Fixed Plus) "P"]
          concl = mkStmt [mkST (Fixed Minus) "S", mkST (Fixed Plus) "P"]
          inf = Inference [p1, p2] concl
      validate inf `shouldSatisfy` isInvalid

    it "De Morgan compound is valid" $ do
      -- -S -(non-A + non-B) ; +(non-A + non-B) +P ∴ -S +P
      let compound = Compound [SignedTerm (Fixed Plus) (Atomic (mkCompTerm "A")) [], SignedTerm (Fixed Plus) (Atomic (mkCompTerm "B")) []]
          p1 = mkStmt [mkST (Fixed Minus) "S", SignedTerm (Fixed Minus) compound []]
          p2 = mkStmt [SignedTerm (Fixed Plus) compound [], mkST (Fixed Plus) "P"]
          concl = mkStmt [mkST (Fixed Minus) "S", mkST (Fixed Plus) "P"]
          inf = Inference [p1, p2] concl
      validate inf `shouldSatisfy` isValid

-- ===================================================================
-- Utilities
-- ===================================================================

isValid :: ValidationResult -> Bool
isValid (Valid _) = True
isValid _ = False

isInvalid :: ValidationResult -> Bool
isInvalid (Invalid _ _) = True
isInvalid _ = False

isLeft' :: Either a b -> Bool
isLeft' (Left _) = True
isLeft' _ = False
