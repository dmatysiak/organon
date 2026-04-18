// Reduction proofs — ported from Organon.Syl.Proof

import { contradictory } from "./proposition";
import { Mood, PropType, Proposition, ProofStep, Syllogism } from "./types";

function swapTerms(p: Proposition): Proposition {
  return { propType: p.propType, subject: p.predicate, predicate: p.subject };
}

function reduceSubaltern(
  parent: Mood,
  strongType: PropType,
  syl: Syllogism,
): ProofStep[] {
  const strongConcl: Proposition = {
    propType: strongType,
    subject: syl.conclusion.subject,
    predicate: syl.conclusion.predicate,
  };
  const parentSyl: Syllogism = {
    major: syl.major,
    minor: syl.minor,
    conclusion: strongConcl,
  };
  return [
    ...reduce(parent, parentSyl),
    { tag: "Subalternation", from: strongConcl, to: syl.conclusion },
  ];
}

export function reduce(mood: Mood, syl: Syllogism): ProofStep[] {
  const { major: maj, minor: min, conclusion: concl } = syl;

  switch (mood) {
    // Figure I: perfect syllogisms
    case Mood.Barbara:
      return [{ tag: "Axiom", mood: Mood.Barbara }];
    case Mood.Celarent:
      return [{ tag: "Axiom", mood: Mood.Celarent }];
    case Mood.Darii:
      return [{ tag: "Axiom", mood: Mood.Darii }];
    case Mood.Ferio:
      return [{ tag: "Axiom", mood: Mood.Ferio }];

    // Figure II
    case Mood.Cesare: {
      const maj2 = swapTerms(maj);
      return [
        { tag: "SimpleConversion", from: maj, to: maj2 },
        { tag: "Axiom", mood: Mood.Celarent },
      ];
    }
    case Mood.Camestres: {
      const min2 = swapTerms(min);
      const derivedConcl: Proposition = {
        propType: PropType.E,
        subject: maj.subject,
        predicate: min2.predicate,
      };
      return [
        { tag: "SimpleConversion", from: min, to: min2 },
        { tag: "MutatePremises" },
        { tag: "Axiom", mood: Mood.Celarent },
        { tag: "SimpleConversion", from: derivedConcl, to: concl },
      ];
    }
    case Mood.Festino: {
      const maj2 = swapTerms(maj);
      return [
        { tag: "SimpleConversion", from: maj, to: maj2 },
        { tag: "Axiom", mood: Mood.Ferio },
      ];
    }
    case Mood.Baroco: {
      const assumed = contradictory(concl);
      const derived: Proposition = {
        propType: PropType.A,
        subject: min.subject,
        predicate: maj.predicate,
      };
      const reductioSyl: Syllogism = {
        major: maj,
        minor: assumed,
        conclusion: derived,
      };
      return [
        {
          tag: "ReductioAdImpossibile",
          targetMood: Mood.Barbara,
          assumed,
          syllogism: reductioSyl,
        },
      ];
    }

    // Figure III
    case Mood.Darapti: {
      const min2: Proposition = {
        propType: PropType.I,
        subject: min.predicate,
        predicate: min.subject,
      };
      return [
        { tag: "ConversionPerAccidens", from: min, to: min2 },
        { tag: "Axiom", mood: Mood.Darii },
      ];
    }
    case Mood.Disamis: {
      const maj2 = swapTerms(maj);
      const derivedConcl: Proposition = {
        propType: PropType.I,
        subject: min.subject,
        predicate: maj2.predicate,
      };
      return [
        { tag: "SimpleConversion", from: maj, to: maj2 },
        { tag: "MutatePremises" },
        { tag: "Axiom", mood: Mood.Darii },
        { tag: "SimpleConversion", from: derivedConcl, to: concl },
      ];
    }
    case Mood.Datisi: {
      const min2 = swapTerms(min);
      return [
        { tag: "SimpleConversion", from: min, to: min2 },
        { tag: "Axiom", mood: Mood.Darii },
      ];
    }
    case Mood.Felapton: {
      const min2: Proposition = {
        propType: PropType.I,
        subject: min.predicate,
        predicate: min.subject,
      };
      return [
        { tag: "ConversionPerAccidens", from: min, to: min2 },
        { tag: "Axiom", mood: Mood.Ferio },
      ];
    }
    case Mood.Bocardo: {
      const assumed = contradictory(concl);
      const derived: Proposition = {
        propType: PropType.A,
        subject: maj.subject,
        predicate: maj.predicate,
      };
      const reductioSyl: Syllogism = {
        major: assumed,
        minor: min,
        conclusion: derived,
      };
      return [
        {
          tag: "ReductioAdImpossibile",
          targetMood: Mood.Barbara,
          assumed,
          syllogism: reductioSyl,
        },
      ];
    }
    case Mood.Ferison: {
      const min2 = swapTerms(min);
      return [
        { tag: "SimpleConversion", from: min, to: min2 },
        { tag: "Axiom", mood: Mood.Ferio },
      ];
    }

    // Figure IV
    case Mood.Bramantip: {
      const derivedConcl: Proposition = {
        propType: PropType.A,
        subject: maj.subject,
        predicate: min.predicate,
      };
      return [
        { tag: "MutatePremises" },
        { tag: "Axiom", mood: Mood.Barbara },
        { tag: "ConversionPerAccidens", from: derivedConcl, to: concl },
      ];
    }
    case Mood.Camenes: {
      const derivedConcl: Proposition = {
        propType: PropType.E,
        subject: maj.subject,
        predicate: min.predicate,
      };
      return [
        { tag: "MutatePremises" },
        { tag: "Axiom", mood: Mood.Celarent },
        { tag: "SimpleConversion", from: derivedConcl, to: concl },
      ];
    }
    case Mood.Dimaris: {
      const derivedConcl: Proposition = {
        propType: PropType.I,
        subject: maj.subject,
        predicate: min.predicate,
      };
      return [
        { tag: "MutatePremises" },
        { tag: "Axiom", mood: Mood.Darii },
        { tag: "SimpleConversion", from: derivedConcl, to: concl },
      ];
    }
    case Mood.Fesapo: {
      const maj2 = swapTerms(maj);
      const min2: Proposition = {
        propType: PropType.I,
        subject: min.predicate,
        predicate: min.subject,
      };
      return [
        { tag: "SimpleConversion", from: maj, to: maj2 },
        { tag: "ConversionPerAccidens", from: min, to: min2 },
        { tag: "Axiom", mood: Mood.Ferio },
      ];
    }
    case Mood.Fresison: {
      const maj2 = swapTerms(maj);
      const min2 = swapTerms(min);
      return [
        { tag: "SimpleConversion", from: maj, to: maj2 },
        { tag: "SimpleConversion", from: min, to: min2 },
        { tag: "Axiom", mood: Mood.Ferio },
      ];
    }

    // Subaltern moods
    case Mood.Barbari:
      return reduceSubaltern(Mood.Barbara, PropType.A, syl);
    case Mood.Celaront:
      return reduceSubaltern(Mood.Celarent, PropType.E, syl);
    case Mood.Cesaro:
      return reduceSubaltern(Mood.Cesare, PropType.E, syl);
    case Mood.Camestrop:
      return reduceSubaltern(Mood.Camestres, PropType.E, syl);
    case Mood.Calemos:
      return reduceSubaltern(Mood.Camenes, PropType.E, syl);
  }
}
