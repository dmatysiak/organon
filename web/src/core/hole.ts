// Hole solver — ported from Organon.Syl.Hole

import { moodSpec, validMoods } from "./tradition";
import { Figure, Mood, PropType, Term, Tradition, termEq } from "./types";

export type TermH =
  | { readonly tag: "ConcreteT"; readonly term: Term }
  | { readonly tag: "HoleT" };

export type PropTypeH =
  | { readonly tag: "ConcretePT"; readonly propType: PropType }
  | { readonly tag: "HolePT" };

export type PropositionH =
  | {
      readonly tag: "PropH";
      readonly propTypeH: PropTypeH;
      readonly subject: TermH;
      readonly predicate: TermH;
    }
  | { readonly tag: "WholePropH" };

export type SyllogismH = {
  readonly majorH: PropositionH;
  readonly minorH: PropositionH;
  readonly conclusionH: PropositionH;
};

export type SolutionProp = {
  readonly solPropType: PropType;
  readonly solSubject: Term | null;
  readonly solPredicate: Term | null;
};

export type Solution = {
  readonly solutionMood: Mood;
  readonly solMajor: SolutionProp;
  readonly solMinor: SolutionProp;
  readonly solConclusion: SolutionProp;
};

enum Role {
  S = "S",
  P = "P",
  M = "M",
}

type RolePair = [Role, Role];
type FigureRoles = [RolePair, RolePair, RolePair];

function figureRoles(fig: Figure): FigureRoles {
  switch (fig) {
    case Figure.FigI:
      return [
        [Role.M, Role.P],
        [Role.S, Role.M],
        [Role.S, Role.P],
      ];
    case Figure.FigII:
      return [
        [Role.P, Role.M],
        [Role.S, Role.M],
        [Role.S, Role.P],
      ];
    case Figure.FigIII:
      return [
        [Role.M, Role.P],
        [Role.M, Role.S],
        [Role.S, Role.P],
      ];
    case Figure.FigIV:
      return [
        [Role.P, Role.M],
        [Role.M, Role.S],
        [Role.S, Role.P],
      ];
  }
}

type Bindings = Map<Role, Term>;

function matchTerm(th: TermH, role: Role, bindings: Bindings): Bindings | null {
  if (th.tag === "HoleT") return bindings;
  const existing = bindings.get(role);
  if (existing === undefined) {
    const next = new Map(bindings);
    next.set(role, th.term);
    return next;
  }
  return termEq(existing, th.term) ? bindings : null;
}

function matchProp(
  propH: PropositionH,
  expectedPT: PropType,
  subjR: Role,
  predR: Role,
  bindings: Bindings,
): Bindings | null {
  if (propH.tag === "WholePropH") return bindings;
  if (
    propH.propTypeH.tag === "ConcretePT" &&
    propH.propTypeH.propType !== expectedPT
  )
    return null;
  const b1 = matchTerm(propH.subject, subjR, bindings);
  if (b1 === null) return null;
  return matchTerm(propH.predicate, predR, b1);
}

export function solve(tradition: Tradition, sylH: SyllogismH): Solution[] {
  const results: Solution[] = [];
  for (const mood of validMoods(tradition)) {
    const spec = moodSpec(mood);
    const fig = spec.moodFigure;
    const [majRoles, minRoles, conRoles] = figureRoles(fig);

    const items: [PropositionH, PropType, Role, Role][] = [
      [sylH.majorH, spec.majorPropType, majRoles[0], majRoles[1]],
      [sylH.minorH, spec.minorPropType, minRoles[0], minRoles[1]],
      [sylH.conclusionH, spec.conclusionPropType, conRoles[0], conRoles[1]],
    ];

    let bindings: Bindings | null = new Map();
    for (const [propH, expectedPT, subjR, predR] of items) {
      bindings = matchProp(propH, expectedPT, subjR, predR, bindings);
      if (bindings === null) break;
    }

    if (bindings !== null) {
      const resolve = (r: Role): Term | null => bindings!.get(r) ?? null;
      results.push({
        solutionMood: mood,
        solMajor: {
          solPropType: spec.majorPropType,
          solSubject: resolve(majRoles[0]),
          solPredicate: resolve(majRoles[1]),
        },
        solMinor: {
          solPropType: spec.minorPropType,
          solSubject: resolve(minRoles[0]),
          solPredicate: resolve(minRoles[1]),
        },
        solConclusion: {
          solPropType: spec.conclusionPropType,
          solSubject: resolve(conRoles[0]),
          solPredicate: resolve(conRoles[1]),
        },
      });
    }
  }
  return results;
}
