// Core types — ported from Organon.Syl.Types

export type Term = {
  readonly termName: string;
  readonly complemented: boolean;
};

export function termEq(a: Term, b: Term): boolean {
  return a.termName === b.termName && a.complemented === b.complemented;
}

export enum PropType {
  A = "A",
  E = "E",
  I = "I",
  O = "O",
}

export const ALL_PROP_TYPES: readonly PropType[] = [
  PropType.A,
  PropType.E,
  PropType.I,
  PropType.O,
];

export type Proposition = {
  readonly propType: PropType;
  readonly subject: Term;
  readonly predicate: Term;
};

export enum Figure {
  FigI = "FigI",
  FigII = "FigII",
  FigIII = "FigIII",
  FigIV = "FigIV",
}

export enum Mood {
  // Figure I (perfect syllogisms)
  Barbara = "Barbara",
  Celarent = "Celarent",
  Darii = "Darii",
  Ferio = "Ferio",
  // Figure II
  Cesare = "Cesare",
  Camestres = "Camestres",
  Festino = "Festino",
  Baroco = "Baroco",
  // Figure III
  Darapti = "Darapti",
  Disamis = "Disamis",
  Datisi = "Datisi",
  Felapton = "Felapton",
  Bocardo = "Bocardo",
  Ferison = "Ferison",
  // Figure IV
  Bramantip = "Bramantip",
  Camenes = "Camenes",
  Dimaris = "Dimaris",
  Fesapo = "Fesapo",
  Fresison = "Fresison",
  // Subaltern moods
  Barbari = "Barbari",
  Celaront = "Celaront",
  Cesaro = "Cesaro",
  Camestrop = "Camestrop",
  Calemos = "Calemos",
}

export const ALL_MOODS: readonly Mood[] = Object.values(Mood);

export enum Tradition {
  Strict = "Strict",
  Traditional = "Traditional",
  Full = "Full",
}

export type Syllogism = {
  readonly major: Proposition;
  readonly minor: Proposition;
  readonly conclusion: Proposition;
};

export type ProofStep =
  | { readonly tag: "Axiom"; readonly mood: Mood }
  | {
      readonly tag: "SimpleConversion";
      readonly from: Proposition;
      readonly to: Proposition;
    }
  | {
      readonly tag: "ConversionPerAccidens";
      readonly from: Proposition;
      readonly to: Proposition;
    }
  | { readonly tag: "MutatePremises" }
  | {
      readonly tag: "ReductioAdImpossibile";
      readonly targetMood: Mood;
      readonly assumed: Proposition;
      readonly syllogism: Syllogism;
    }
  | {
      readonly tag: "Subalternation";
      readonly from: Proposition;
      readonly to: Proposition;
    };

/** Infer the figure from a syllogism by finding the middle term. */
export function figure(syl: Syllogism): Figure | null {
  const majS = syl.major.subject;
  const majP = syl.major.predicate;
  const minS = syl.minor.subject;
  const minP = syl.minor.predicate;
  const majorTerm = syl.conclusion.predicate;
  const minorTerm = syl.conclusion.subject;

  if (termEq(majS, minP) && termEq(majP, majorTerm) && termEq(minS, minorTerm))
    return Figure.FigI;
  if (termEq(majP, minP) && termEq(majS, majorTerm) && termEq(minS, minorTerm))
    return Figure.FigII;
  if (termEq(majS, minS) && termEq(majP, majorTerm) && termEq(minP, minorTerm))
    return Figure.FigIII;
  if (termEq(majP, minS) && termEq(majS, majorTerm) && termEq(minP, minorTerm))
    return Figure.FigIV;
  return null;
}

export function isAffirmative(pt: PropType): boolean {
  return pt === PropType.A || pt === PropType.I;
}

export function isNegative(pt: PropType): boolean {
  return !isAffirmative(pt);
}

export function isUniversal(pt: PropType): boolean {
  return pt === PropType.A || pt === PropType.E;
}

export function isParticular(pt: PropType): boolean {
  return !isUniversal(pt);
}
