module Organon.Syl.Proof
  ( reduce,
  )
where

import Organon.Syl.Proposition (contradictory)
import Organon.Syl.Types

-- | Construct a reduction proof for a valid syllogism of the given mood.
-- Returns the sequence of proof steps that reduce it to a Figure I syllogism.
--
-- Precondition: the syllogism must actually be valid in the given mood.
-- If called on an invalid syllogism, the proof steps will be nonsensical.
reduce :: Mood -> Syllogism -> [ProofStep]
-- Figure I: perfect syllogisms, accepted as axioms.
reduce Barbara _ = [Axiom Barbara]
reduce Celarent _ = [Axiom Celarent]
reduce Darii _ = [Axiom Darii]
reduce Ferio _ = [Axiom Ferio]
-- Figure II

-- Cesare (EAE-2) → Celarent: simple convert major.
--   E(P,M), A(S,M) → E(S,P)
--   Convert E(P,M) → E(M,P), then Celarent.
reduce Cesare (Syllogism maj _ _) =
  let maj' = swapTerms maj
   in [SimpleConversion maj maj', Axiom Celarent]
-- Camestres (AEE-2) → Celarent: simple convert minor, mutate, Celarent,
--   simple convert conclusion.
--   A(P,M), E(S,M) → E(S,P)
reduce Camestres (Syllogism maj min_ concl) =
  let min' = swapTerms min_
      derivedConcl = Proposition E (subject maj) (predicate min')
   in [ SimpleConversion min_ min',
        MutatePremises,
        Axiom Celarent,
        SimpleConversion derivedConcl concl
      ]
-- Festino (EIO-2) → Ferio: simple convert major.
--   E(P,M), I(S,M) → O(S,P)
--   Convert E(P,M) → E(M,P), then Ferio.
reduce Festino (Syllogism maj _ _) =
  let maj' = swapTerms maj
   in [SimpleConversion maj maj', Axiom Ferio]
-- Baroco (AOO-2) → Barbara: reductio ad impossibile.
--   A(P,M), O(S,M) → O(S,P)
--   Assume A(S,P) [contradictory of O(S,P)].
--   Barbara: A(P,M), A(S,P) → A(S,M), which contradicts O(S,M).
reduce Baroco (Syllogism maj min_ concl) =
  let assumed = contradictory concl
      derived = Proposition A (subject min_) (predicate maj)
      reductioSyl = Syllogism maj assumed derived
   in [ReductioAdImpossibile Barbara assumed reductioSyl]
-- Figure III

-- Darapti (AAI-3) → Darii: convert minor per accidens.
--   A(M,P), A(M,S) → I(S,P)
--   Convert A(M,S) → I(S,M), then Darii.
reduce Darapti (Syllogism _ min_ _) =
  let min' = Proposition I (predicate min_) (subject min_)
   in [ConversionPerAccidens min_ min', Axiom Darii]
-- Disamis (IAI-3) → Darii: simple convert major, mutate, Darii,
--   simple convert conclusion.
--   I(M,P), A(M,S) → I(S,P)
reduce Disamis (Syllogism maj min_ concl) =
  let maj' = swapTerms maj
      derivedConcl = Proposition I (subject min_) (predicate maj')
   in [ SimpleConversion maj maj',
        MutatePremises,
        Axiom Darii,
        SimpleConversion derivedConcl concl
      ]
-- Datisi (AII-3) → Darii: simple convert minor.
--   A(M,P), I(M,S) → I(S,P)
--   Convert I(M,S) → I(S,M), then Darii.
reduce Datisi (Syllogism _ min_ _) =
  let min' = swapTerms min_
   in [SimpleConversion min_ min', Axiom Darii]
-- Felapton (EAO-3) → Ferio: convert minor per accidens.
--   E(M,P), A(M,S) → O(S,P)
--   Convert A(M,S) → I(S,M), then Ferio.
reduce Felapton (Syllogism _ min_ _) =
  let min' = Proposition I (predicate min_) (subject min_)
   in [ConversionPerAccidens min_ min', Axiom Ferio]
-- Bocardo (OAO-3) → Barbara: reductio ad impossibile.
--   O(M,P), A(M,S) → O(S,P)
--   Assume A(S,P) [contradictory of O(S,P)].
--   Barbara: A(S,P), A(M,S) → A(M,P), which contradicts O(M,P).
reduce Bocardo (Syllogism maj min_ concl) =
  let assumed = contradictory concl
      derived = Proposition A (subject maj) (predicate maj)
      reductioSyl = Syllogism assumed min_ derived
   in [ReductioAdImpossibile Barbara assumed reductioSyl]
-- Ferison (EIO-3) → Ferio: simple convert minor.
--   E(M,P), I(M,S) → O(S,P)
--   Convert I(M,S) → I(S,M), then Ferio.
reduce Ferison (Syllogism _ min_ _) =
  let min' = swapTerms min_
   in [SimpleConversion min_ min', Axiom Ferio]
-- Figure IV

-- Bramantip (AAI-4) → Barbara: mutate, Barbara, convert conclusion per accidens.
--   A(P,M), A(M,S) → I(S,P)
reduce Bramantip (Syllogism maj min_ concl) =
  let derivedConcl = Proposition A (subject maj) (predicate min_)
   in [ MutatePremises,
        Axiom Barbara,
        ConversionPerAccidens derivedConcl concl
      ]
-- Camenes (AEE-4) → Celarent: mutate, Celarent, simple convert conclusion.
--   A(P,M), E(M,S) → E(S,P)
reduce Camenes (Syllogism maj min_ concl) =
  let derivedConcl = Proposition E (subject maj) (predicate min_)
   in [ MutatePremises,
        Axiom Celarent,
        SimpleConversion derivedConcl concl
      ]
-- Dimaris (IAI-4) → Darii: mutate, Darii, simple convert conclusion.
--   I(P,M), A(M,S) → I(S,P)
reduce Dimaris (Syllogism maj min_ concl) =
  let derivedConcl = Proposition I (subject maj) (predicate min_)
   in [ MutatePremises,
        Axiom Darii,
        SimpleConversion derivedConcl concl
      ]
-- Fesapo (EAO-4) → Ferio: simple convert major, convert minor per accidens.
--   E(P,M), A(M,S) → O(S,P)
reduce Fesapo (Syllogism maj min_ _) =
  let maj' = swapTerms maj
      min' = Proposition I (predicate min_) (subject min_)
   in [ SimpleConversion maj maj',
        ConversionPerAccidens min_ min',
        Axiom Ferio
      ]
-- Fresison (EIO-4) → Ferio: simple convert major, simple convert minor.
--   E(P,M), I(M,S) → O(S,P)
reduce Fresison (Syllogism maj min_ _) =
  let maj' = swapTerms maj
      min' = swapTerms min_
   in [ SimpleConversion maj maj',
        SimpleConversion min_ min',
        Axiom Ferio
      ]
-- Subaltern moods: reduce the parent mood, then subalternate the conclusion.

reduce Barbari syl =
  reduceSubaltern Barbara A syl
reduce Celaront syl =
  reduceSubaltern Celarent E syl
reduce Cesaro syl =
  reduceSubaltern Cesare E syl
reduce Camestrop syl =
  reduceSubaltern Camestres E syl
reduce Calemos syl =
  reduceSubaltern Camenes E syl

-- Helpers

-- | Swap subject and predicate of a proposition, keeping the type.
swapTerms :: Proposition -> Proposition
swapTerms (Proposition t s p) = Proposition t p s

-- | Reduce a subaltern mood by reducing the parent mood with a stronger
-- conclusion, then subalternating.
reduceSubaltern :: Mood -> PropType -> Syllogism -> [ProofStep]
reduceSubaltern parent strongType (Syllogism maj min_ concl) =
  let strongConcl = Proposition strongType (subject concl) (predicate concl)
      parentSyl = Syllogism maj min_ strongConcl
   in reduce parent parentSyl ++ [Subalternation strongConcl concl]
