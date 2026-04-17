{-# LANGUAGE OverloadedStrings #-}

module Organon.Syl.Proposition
  ( simpleConversion,
    conversionPerAccidens,
    obversion,
    contraposition,
    contradictory,
    negateTerm,
  )
where

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Organon.Syl.Types
  ( PropType (A, E, I, O),
    Proposition (Proposition),
    Term (Term),
  )

-- | Negate a term by toggling its complement flag.
negateTerm :: Term -> Term
negateTerm (Term n c) = Term n (not c)

-- | Simple conversion ('s' in the mnemonics).
-- Swap subject and predicate, preserving the proposition type.
-- Valid only for E and I propositions.
--   E: "no S is P"   → "no P is S"
--   I: "some S is P" → "some P is S"
simpleConversion :: Proposition -> Maybe Proposition
simpleConversion (Proposition E s p) = Just (Proposition E p s)
simpleConversion (Proposition I s p) = Just (Proposition I p s)
simpleConversion _ = Nothing

-- | Conversion per accidens ('p' in the mnemonics).
-- Swap subject and predicate, weaken the quantity.
-- Requires existential import.
--   A: "every S is P" → "some P is S"
--   E: "no S is P"  → "some P is not S"
conversionPerAccidens :: Proposition -> Maybe Proposition
conversionPerAccidens (Proposition A s p) = Just (Proposition I p s)
conversionPerAccidens (Proposition E s p) = Just (Proposition O p s)
conversionPerAccidens _ = Nothing

-- | Obversion: keep the subject, flip the quality, negate the predicate.
-- Valid for all four proposition types.
--   A: "every S is P"      → "no S is non-P"
--   E: "no S is P"         → "every S is non-P"
--   I: "some S is P"       → "some S is not non-P"
--   O: "some S is not P"   → "some S is non-P"
obversion :: Proposition -> Proposition
obversion (Proposition A s p) = Proposition E s (negateTerm p)
obversion (Proposition E s p) = Proposition A s (negateTerm p)
obversion (Proposition I s p) = Proposition O s (negateTerm p)
obversion (Proposition O s p) = Proposition I s (negateTerm p)

-- | The contradictory of a proposition: opposite quality and quantity,
-- same subject and predicate. A↔O, E↔I.
contradictory :: Proposition -> Proposition
contradictory (Proposition A s p) = Proposition O s p
contradictory (Proposition E s p) = Proposition I s p
contradictory (Proposition I s p) = Proposition E s p
contradictory (Proposition O s p) = Proposition A s p

-- | Contraposition: obvert → convert → obvert.
-- Tries simple conversion first, falls back to conversion per accidens.
--   A → A, E → O (by limitation), O → O, I → Nothing.
contraposition :: Proposition -> Maybe Proposition
contraposition p =
  let obverted = obversion p
   in (simpleConversion obverted <|> conversionPerAccidens obverted)
        <&> obversion
