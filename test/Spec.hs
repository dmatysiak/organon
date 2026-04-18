module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Organon.Syl.Check
import Organon.Syl.Document
import Organon.Syl.Hole
import Organon.Syl.Pretty
import Organon.Syl.Proof
import Organon.Syl.Proposition
import Organon.Syl.Parser (parseProposition, parsePropositionH, parseSyllogism, parseSyllogismH)
import Organon.Syl.Tradition
import Organon.Syl.Types
import Organon.Syl.Validity
import Test.Hspec

-- Test terms
m, s, p :: Term
m = Term "M" False
s = Term "S" False
p = Term "P" False

main :: IO ()
main = hspec $ do
  describe "Tradition" $ do
    it "Strict tradition has 15 moods" $
      length (validMoods Strict) `shouldBe` 15

    it "Traditional tradition has 19 moods" $
      length (validMoods Traditional) `shouldBe` 19

    it "Full tradition has 24 moods" $
      length (validMoods Full) `shouldBe` 24

    it "Darapti requires existential import" $
      requiresExistentialImport Darapti `shouldBe` True

    it "Barbara does not require existential import" $
      requiresExistentialImport Barbara `shouldBe` False

    it "Barbari is subaltern" $
      isSubaltern Barbari `shouldBe` True

    it "Barbara is not subaltern" $
      isSubaltern Barbara `shouldBe` False

  describe "Figure inference" $ do
    it "detects Figure I (Barbara: AAA-1)" $
      let syl =
            Syllogism
              (Proposition A m p) -- Every M is P
              (Proposition A s m) -- Every S is M
              (Proposition A s p) -- Every S is P
       in figure syl `shouldBe` Just FigI

    it "detects Figure II (Cesare: EAE-2)" $
      let syl =
            Syllogism
              (Proposition E p m) -- No P is M
              (Proposition A s m) -- Every S is M
              (Proposition E s p) -- No S is P
       in figure syl `shouldBe` Just FigII

    it "detects Figure III (Darapti: AAI-3)" $
      let syl =
            Syllogism
              (Proposition A m p) -- Every M is P
              (Proposition A m s) -- Every M is S
              (Proposition I s p) -- Some S is P
       in figure syl `shouldBe` Just FigIII

    it "detects Figure IV (Bramantip: AAI-4)" $
      let syl =
            Syllogism
              (Proposition A p m) -- Every P is M
              (Proposition A m s) -- Every M is S
              (Proposition I s p) -- Some S is P
       in figure syl `shouldBe` Just FigIV

    it "returns Nothing for malformed syllogism" $
      let syl =
            Syllogism
              (Proposition A s p)
              (Proposition A s p)
              (Proposition A s p)
       in figure syl `shouldBe` Nothing

  describe "Validity" $ do
    it "validates Barbara" $
      let syl =
            Syllogism
              (Proposition A m p)
              (Proposition A s m)
              (Proposition A s p)
       in validate Full syl `shouldBe` Valid Barbara

    it "validates Celarent" $
      let syl =
            Syllogism
              (Proposition E m p)
              (Proposition A s m)
              (Proposition E s p)
       in validate Full syl `shouldBe` Valid Celarent

    it "validates Darii" $
      let syl =
            Syllogism
              (Proposition A m p)
              (Proposition I s m)
              (Proposition I s p)
       in validate Full syl `shouldBe` Valid Darii

    it "validates Ferio" $
      let syl =
            Syllogism
              (Proposition E m p)
              (Proposition I s m)
              (Proposition O s p)
       in validate Full syl `shouldBe` Valid Ferio

    it "validates Cesare" $
      let syl =
            Syllogism
              (Proposition E p m)
              (Proposition A s m)
              (Proposition E s p)
       in validate Full syl `shouldBe` Valid Cesare

    it "validates Baroco" $
      let syl =
            Syllogism
              (Proposition A p m)
              (Proposition O s m)
              (Proposition O s p)
       in validate Full syl `shouldBe` Valid Baroco

    it "validates Bocardo" $
      let syl =
            Syllogism
              (Proposition O m p)
              (Proposition A m s)
              (Proposition O s p)
       in validate Full syl `shouldBe` Valid Bocardo

    it "rejects invalid syllogism" $
      let syl =
            Syllogism
              (Proposition A m p)
              (Proposition A s m)
              (Proposition E s p) -- wrong conclusion type
       in case validate Full syl of
            Invalid _ -> True `shouldBe` True
            _ -> expectationFailure "Should be invalid"

    it "Darapti invalid under Strict" $
      let syl =
            Syllogism
              (Proposition A m p)
              (Proposition A m s)
              (Proposition I s p)
       in case validate Strict syl of
            Invalid _ -> True `shouldBe` True
            _ -> expectationFailure "Darapti should be invalid under Strict"

    it "Darapti valid under Traditional" $
      let syl =
            Syllogism
              (Proposition A m p)
              (Proposition A m s)
              (Proposition I s p)
       in validate Traditional syl `shouldBe` Valid Darapti

    it "Barbari valid under Full but not Strict" $ do
      let syl =
            Syllogism
              (Proposition A m p)
              (Proposition A s m)
              (Proposition I s p)
      validate Full syl `shouldBe` Valid Barbari
      case validate Strict syl of
        Invalid _ -> True `shouldBe` True
        _ -> expectationFailure "Barbari should be invalid under Strict"

    it "validates with swapped premises" $ do
      let syl =
            Syllogism
              (Proposition I s m) -- minor premise in major slot
              (Proposition A m p) -- major premise in minor slot
              (Proposition I s p)
      case validate Full syl of
        ValidSwapped mood _ -> mood `shouldBe` Darii
        Valid _ -> expectationFailure "Should be ValidSwapped, not Valid"
        Invalid msg -> expectationFailure $ "Should be valid: " ++ T.unpack msg

  describe "Proposition operations" $ do
    it "simple conversion of E" $
      simpleConversion (Proposition E s p)
        `shouldBe` Just (Proposition E p s)

    it "simple conversion of I" $
      simpleConversion (Proposition I s p)
        `shouldBe` Just (Proposition I p s)

    it "simple conversion of A fails" $
      simpleConversion (Proposition A s p) `shouldBe` Nothing

    it "conversion per accidens of A" $
      conversionPerAccidens (Proposition A s p)
        `shouldBe` Just (Proposition I p s)

    it "conversion per accidens of E" $
      conversionPerAccidens (Proposition E s p)
        `shouldBe` Just (Proposition O p s)

    it "obversion of A" $
      obversion (Proposition A s p)
        `shouldBe` Proposition E s (Term "P" True)

    it "obversion is self-inverse" $
      let prop = Proposition A s p
       in obversion (obversion prop) `shouldBe` prop

    it "negateTerm toggles complement" $ do
      negateTerm (Term "P" False) `shouldBe` Term "P" True
      negateTerm (Term "P" True) `shouldBe` Term "P" False

    it "contradictory of A is O" $
      contradictory (Proposition A s p) `shouldBe` Proposition O s p

    it "contradictory is self-inverse" $
      let prop = Proposition E s p
       in contradictory (contradictory prop) `shouldBe` prop

    it "contraposition of A" $
      contraposition (Proposition A s p)
        `shouldBe` Just (Proposition A (Term "P" True) (Term "S" True))

    it "contraposition of I is Nothing" $
      contraposition (Proposition I s p) `shouldBe` Nothing

  describe "Proof reduction" $ do
    it "Figure I moods are axioms" $ do
      let syl = Syllogism (Proposition A m p) (Proposition A s m) (Proposition A s p)
      reduce Barbara syl `shouldBe` [Axiom Barbara]

    it "Cesare reduces to Celarent" $ do
      let syl = Syllogism (Proposition E p m) (Proposition A s m) (Proposition E s p)
          steps = reduce Cesare syl
      last steps `shouldBe` Axiom Celarent

    it "Baroco reduces via reductio" $ do
      let syl = Syllogism (Proposition A p m) (Proposition O s m) (Proposition O s p)
          steps = reduce Baroco syl
      any isReductio steps `shouldBe` True

    it "Bocardo reduces via reductio" $ do
      let syl = Syllogism (Proposition O m p) (Proposition A m s) (Proposition O s p)
          steps = reduce Bocardo syl
      any isReductio steps `shouldBe` True

    it "Barbari reduces via subalternation" $ do
      let syl = Syllogism (Proposition A m p) (Proposition A s m) (Proposition I s p)
          steps = reduce Barbari syl
      any isSubalternStep steps `shouldBe` True

  describe "Pretty printing" $ do
    it "prettyProposition renders A correctly" $
      prettyProposition (Proposition A s p) `shouldBe` "every S is P"

    it "prettyProposition renders O correctly" $
      prettyProposition (Proposition O s p) `shouldBe` "some S is not P"

    it "prettyTerm renders complemented term" $
      prettyTerm (Term "P" True) `shouldBe` "non-P"

  describe "Parser" $ do
    it "parses A proposition" $
      parseProposition "Every S is P"
        `shouldBe` Right (Proposition A s p)

    it "parses E proposition" $
      parseProposition "No S is P"
        `shouldBe` Right (Proposition E s p)

    it "parses I proposition" $
      parseProposition "Some S is P"
        `shouldBe` Right (Proposition I s p)

    it "parses O proposition" $
      parseProposition "Some S is not P"
        `shouldBe` Right (Proposition O s p)

    it "parses complemented term" $
      parseProposition "Every non-S is P"
        `shouldBe` Right (Proposition A (Term "S" True) p)

    it "is case-insensitive" $
      parseProposition "every S is P"
        `shouldBe` Right (Proposition A s p)

    it "parses syllogism with semicolons" $
      parseSyllogism "Every M is P; Every S is M; Every S is P"
        `shouldBe` Right
          ( Syllogism
              (Proposition A m p)
              (Proposition A s m)
              (Proposition A s p)
          )

    it "parser round-trips with pretty printer" $ do
      let prop = Proposition A s p
      parseProposition (prettyProposition prop) `shouldBe` Right prop

  describe "Holes" $ do
    it "solve finds Barbara for conclusion hole" $ do
      let sylH =
            SylH
              (PropH (ConcretePT A) (ConcreteT m) (ConcreteT p))
              (PropH (ConcretePT A) (ConcreteT s) (ConcreteT m))
              WholePropH
          solutions = solve Full sylH
          moods = map solutionMood solutions
      Barbara `shouldSatisfy` (`elem` moods)

    it "solve finds Celarent for conclusion hole" $ do
      let sylH =
            SylH
              (PropH (ConcretePT E) (ConcreteT m) (ConcreteT p))
              (PropH (ConcretePT A) (ConcreteT s) (ConcreteT m))
              WholePropH
          solutions = solve Full sylH
          moods = map solutionMood solutions
      Celarent `shouldSatisfy` (`elem` moods)

    it "solve finds missing minor premise" $ do
      let sylH =
            SylH
              (PropH (ConcretePT A) (ConcreteT m) (ConcreteT p))
              WholePropH
              (PropH (ConcretePT A) (ConcreteT s) (ConcreteT p))
          solutions = solve Full sylH
          moods = map solutionMood solutions
      Barbara `shouldSatisfy` (`elem` moods)

    it "solve respects PropType constraints" $ do
      let sylH =
            SylH
              (PropH (ConcretePT A) (ConcreteT m) (ConcreteT p))
              (PropH (ConcretePT A) (ConcreteT s) (ConcreteT m))
              (PropH (ConcretePT E) HoleT HoleT)
          solutions = solve Full sylH
      solutions `shouldBe` []

    it "solve with quantifier hole finds matching moods" $ do
      let sylH =
            SylH
              (PropH HolePT (ConcreteT m) (ConcreteT p))
              (PropH (ConcretePT A) (ConcreteT s) (ConcreteT m))
              (PropH HolePT (ConcreteT s) (ConcreteT p))
          solutions = solve Full sylH
      length solutions `shouldSatisfy` (> 0)
      Barbara `shouldSatisfy` (`elem` map solutionMood solutions)

    it "solve respects tradition" $ do
      let sylH =
            SylH
              (PropH (ConcretePT A) (ConcreteT m) (ConcreteT p))
              (PropH (ConcretePT A) (ConcreteT m) (ConcreteT s))
              WholePropH
          strictSols = solve Strict sylH
          tradSols = solve Traditional sylH
      Darapti `shouldSatisfy` (`notElem` map solutionMood strictSols)
      Darapti `shouldSatisfy` (`elem` map solutionMood tradSols)

    it "parses hole proposition" $
      parsePropositionH "?" `shouldBe` Right WholePropH

    it "parses quantifier hole" $
      parsePropositionH "? S is P"
        `shouldBe` Right (PropH HolePT (ConcreteT s) (ConcreteT p))

    it "parses term holes" $
      parsePropositionH "Every ? is P"
        `shouldBe` Right (PropH (ConcretePT A) HoleT (ConcreteT p))

    it "parses syllogism with conclusion hole" $
      case parseSyllogismH "Every M is P; Every S is M; ?" of
        Right (SylH _ _ WholePropH) -> True `shouldBe` True
        _ -> expectationFailure "Should parse conclusion hole"

    it "prettyTermH renders hole" $
      prettyTermH HoleT `shouldBe` "?"

    it "prettyPropositionH renders whole hole" $
      prettyPropositionH WholePropH `shouldBe` "?"

    it "prettyPropositionH renders quantifier hole" $
      prettyPropositionH (PropH HolePT (ConcreteT s) (ConcreteT p))
        `shouldBe` "? S is P"

  describe "Document parser" $ do
    it "parses a single proof block" $
      case parseDocument "proof Barbara\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          docTradition doc `shouldBe` Nothing
          length (docProofs doc) `shouldBe` 1
          let block = locValue (head (docProofs doc))
          locValue (proofName block) `shouldBe` "Barbara"
          length (proofPremises block) `shouldBe` 2
        Left err -> expectationFailure (T.unpack err)

    it "parses tradition directive" $
      case parseDocument "tradition Strict\n\nproof Barbara\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc ->
          fmap locValue (docTradition doc) `shouldBe` Just Strict
        Left err -> expectationFailure (T.unpack err)

    it "parses @reference premises" $
      case parseDocument "proof A\nEvery M is P\nEvery S is M\n∴ Every S is P\n\nproof B\n@A\nNo S is P\n∴ No S is P\n" of
        Right doc -> do
          length (docProofs doc) `shouldBe` 2
          let block2 = locValue (docProofs doc !! 1)
          case locValue (head (proofPremises block2)) of
            PremiseRef _ name -> name `shouldBe` "A"
            PremiseProp _ -> expectationFailure "Expected PremiseRef"
        Left err -> expectationFailure (T.unpack err)

    it "parses 'therefore' as conclusion marker" $
      case parseDocument "proof Barbara\nEvery M is P\nEvery S is M\ntherefore Every S is P\n" of
        Right doc -> length (docProofs doc) `shouldBe` 1
        Left err -> expectationFailure (T.unpack err)

    it "rejects malformed document" $
      case parseDocument "not a valid document\n" of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should have failed to parse"

    it "handles line comments" $
      case parseDocument "-- A comment\nproof Barbara\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> length (docProofs doc) `shouldBe` 1
        Left err -> expectationFailure (T.unpack err)

    it "parses open directives" $
      case parseDocument "open Basics\n\nproof Step1\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          length (docOpens doc) `shouldBe` 1
          locValue (head (docOpens doc)) `shouldBe` "Basics"
        Left err -> expectationFailure (T.unpack err)

    it "parses multiple open directives" $
      case parseDocument "open Basics\nopen Advanced\n\nproof Step1\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          length (docOpens doc) `shouldBe` 2
          map locValue (docOpens doc) `shouldBe` ["Basics", "Advanced"]
        Left err -> expectationFailure (T.unpack err)

    it "parses qualified @Ns.Name references" $
      case parseDocument "proof Step1\n@Basics.Barbara\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          case locValue (head (proofPremises block)) of
            PremiseRef (Just ns) name -> do
              ns `shouldBe` "Basics"
              name `shouldBe` "Barbara"
            _ -> expectationFailure "Expected qualified PremiseRef"
        Left err -> expectationFailure (T.unpack err)

    it "parses unqualified @Name as PremiseRef Nothing" $
      case parseDocument "proof Step1\n@Barbara\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          case locValue (head (proofPremises block)) of
            PremiseRef Nothing name -> name `shouldBe` "Barbara"
            _ -> expectationFailure "Expected unqualified PremiseRef"
        Left err -> expectationFailure (T.unpack err)

  describe "Document checker" $ do
    it "checks a valid Barbara syllogism" $
      case parseDocument "proof Barbara\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldBe` []
          length (checkProofs result) `shouldBe` 1
          checkedMood (head (checkProofs result)) `shouldBe` Barbara
        Left err -> expectationFailure (T.unpack err)

    it "reports error for invalid syllogism" $
      case parseDocument "proof Bad\nEvery M is P\nEvery S is P\n∴ Every S is M\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` (not . null)
          diagSeverity (head (checkDiagnostics result)) `shouldBe` Organon.Syl.Check.Error
        Left err -> expectationFailure (T.unpack err)

    it "resolves @references to prior conclusions" $
      case parseDocument "proof Step1\nEvery M is P\nEvery S is M\n∴ Every S is P\n\nproof Step2\n@Step1\nEvery P is M\n∴ Every S is M\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          -- Step1 valid, Step2 uses its conclusion
          length (checkProofs result) `shouldBe` 2
        Left err -> expectationFailure (T.unpack err)

    it "reports error for unknown reference" $
      case parseDocument "proof Bad\n@NoSuchProof\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` any (\d -> "Unknown reference" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "reports error for duplicate proof name" $
      case parseDocument "proof Dup\nEvery M is P\nEvery S is M\n∴ Every S is P\n\nproof Dup\nNo M is P\nEvery S is M\n∴ No S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` any (\d -> "Duplicate" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "reports error for wrong premise count" $
      case parseDocument "proof Bad\nEvery M is P\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` any (\d -> "Expected 2 premises" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "respects tradition directive" $
      case parseDocument "tradition Strict\n\nproof Bramantip\nEvery P is M\nEvery M is S\n∴ Some S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          -- Bramantip requires existential import, not in Strict
          checkDiagnostics result `shouldSatisfy` (not . null)
        Left err -> expectationFailure (T.unpack err)

    it "produces hover items for proof names" $
      case parseDocument "proof Barbara\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHovers result `shouldSatisfy` any (\h -> "Barbara" `elem` words' (hoverText h))
        Left err -> expectationFailure (T.unpack err)

    it "produces hover items for @references" $
      case parseDocument "proof Step1\nEvery M is P\nEvery S is M\n∴ Every S is P\n\nproof Step2\n@Step1\nEvery P is M\n∴ Every S is M\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHovers result `shouldSatisfy` any (\h -> "every S is P" `elem` segments (hoverText h))
        Left err -> expectationFailure (T.unpack err)

    it "resolves qualified @Ns.Name from external context" $
      let ext =
            ExternalContext $
              Map.singleton
                "Basics"
                (NamespaceEntry
                  "basics.syl"
                  (Map.singleton "Barbara" (Proposition A m p))
                  (Map.singleton "Barbara" (SrcPos 1 7, SrcPos 1 14)))
       in case parseDocument "proof Step1\n@Basics.Barbara\nEvery S is M\n∴ Every S is P\n" of
            Right doc -> do
              let result = checkDocument ext doc
              checkDiagnostics result `shouldSatisfy` null
            Left err -> expectationFailure (T.unpack err)

    it "resolves unqualified @Name via open" $
      let ext =
            ExternalContext $
              Map.singleton
                "Basics"
                (NamespaceEntry
                  "basics.syl"
                  (Map.singleton "Barbara" (Proposition A m p))
                  (Map.singleton "Barbara" (SrcPos 1 7, SrcPos 1 14)))
       in case parseDocument "open Basics\n\nproof Step1\n@Barbara\nEvery S is M\n∴ Every S is P\n" of
            Right doc -> do
              let result = checkDocument ext doc
              checkDiagnostics result `shouldSatisfy` null
            Left err -> expectationFailure (T.unpack err)

    it "reports unknown namespace in qualified ref" $
      case parseDocument "proof Step1\n@NoSuch.Barbara\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` any (\d -> "Unknown namespace" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "reports unknown namespace in open directive" $
      case parseDocument "open NoSuch\n\nproof Step1\nEvery M is P\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` any (\d -> "Unknown namespace" `T.isInfixOf` diagMessage d)
        Left err -> expectationFailure (T.unpack err)

    it "reports ambiguous unqualified ref across opens" $
      let ext =
            ExternalContext $
              Map.fromList
                [ ("A", NamespaceEntry "a.syl" (Map.singleton "Foo" (Proposition A s p)) Map.empty),
                  ("B", NamespaceEntry "b.syl" (Map.singleton "Foo" (Proposition E s p)) Map.empty)
                ]
       in case parseDocument "open A\nopen B\n\nproof Step1\n@Foo\nEvery S is M\n∴ Every S is P\n" of
            Right doc -> do
              let result = checkDocument ext doc
              checkDiagnostics result `shouldSatisfy` any (\d -> "Ambiguous" `T.isInfixOf` diagMessage d)
            Left err -> expectationFailure (T.unpack err)

    it "parses hole conclusion" $
      case parseDocument "proof Test\nEvery M is P\nEvery S is M\n∴ ?\n" of
        Right doc -> do
          length (docProofs doc) `shouldBe` 1
          let block = locValue (head (docProofs doc))
          locValue (proofConclusion block) `shouldBe` WholePropH
        Left err -> expectationFailure (T.unpack err)

    it "produces hole fills for hole conclusion" $
      case parseDocument "proof Test\nEvery M is P\nEvery S is M\n∴ ?\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHoleFills result `shouldSatisfy` (not . null)
          checkHoleFills result `shouldSatisfy` any (\f -> holeFillMood f == Barbara)
        Left err -> expectationFailure (T.unpack err)

    it "emits warning diagnostic for hole" $
      case parseDocument "proof Test\nEvery M is P\nEvery S is M\n∴ ?\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkDiagnostics result `shouldSatisfy` any (\d -> "solution" `T.isInfixOf` diagMessage d)
          diagSeverity (head (checkDiagnostics result)) `shouldBe` Warning
        Left err -> expectationFailure (T.unpack err)

    it "parses term hole in conclusion" $
      case parseDocument "proof Test\nEvery M is P\nEvery S is M\n∴ Every ? is P\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          locValue (proofConclusion block) `shouldBe` PropH (ConcretePT A) HoleT (ConcreteT p)
        Left err -> expectationFailure (T.unpack err)

    it "parses quantifier hole in conclusion" $
      case parseDocument "proof Test\nEvery M is P\nEvery S is M\n∴ ? S is P\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          locValue (proofConclusion block) `shouldBe` PropH HolePT (ConcreteT s) (ConcreteT p)
        Left err -> expectationFailure (T.unpack err)

    it "parses term hole in premise" $
      case parseDocument "proof Test\nEvery ? is P\nEvery S is M\n∴ ?\n" of
        Right doc -> do
          let block = locValue (head (docProofs doc))
          locValue (head (proofPremises block)) `shouldBe` PremiseHole (PropH (ConcretePT A) HoleT (ConcreteT p))
        Left err -> expectationFailure (T.unpack err)

    it "produces fills for term hole in premise" $
      case parseDocument "proof Test\nEvery ? is P\nEvery S is ?\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHoleFills result `shouldSatisfy` (not . null)
          checkHoleFills result `shouldSatisfy` any (\f -> holeFillMood f == Barbara)
        Left err -> expectationFailure (T.unpack err)

    it "produces fills for whole premise hole" $
      case parseDocument "proof Test\n?\nEvery S is M\n∴ Every S is P\n" of
        Right doc -> do
          let result = checkDocument (ExternalContext Map.empty) doc
          checkHoleFills result `shouldSatisfy` (not . null)
        Left err -> expectationFailure (T.unpack err)

-- Helpers

isReductio :: ProofStep -> Bool
isReductio (ReductioAdImpossibile _ _ _) = True
isReductio _ = False

isSubalternStep :: ProofStep -> Bool
isSubalternStep (Subalternation _ _) = True
isSubalternStep _ = False

words' :: Text -> [Text]
words' = T.words

segments :: Text -> [Text]
segments = T.lines
