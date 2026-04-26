// Core TFL types — ported from Organon.Tfl.Types

export enum Sign {
  Plus = "Plus",
  Minus = "Minus",
}

export type WildSign =
  | { readonly tag: "Wild" }
  | { readonly tag: "Fixed"; readonly sign: Sign };

export const Wild: WildSign = { tag: "Wild" };
export function Fixed(s: Sign): WildSign {
  return { tag: "Fixed", sign: s };
}

export type Term = {
  readonly termName: string;
  readonly complemented: boolean;
};

export function termEq(a: Term, b: Term): boolean {
  return a.termName === b.termName && a.complemented === b.complemented;
}

// | A term expression: either an atomic term or a conjunction of signed terms.
export type TermExpr =
  | { readonly tag: "Atomic"; readonly term: Term }
  | { readonly tag: "Compound"; readonly elements: readonly SignedTerm[] };

export function Atomic(t: Term): TermExpr {
  return { tag: "Atomic", term: t };
}

export function Compound(sts: readonly SignedTerm[]): TermExpr {
  return { tag: "Compound", elements: sts };
}

export function termExprEq(a: TermExpr, b: TermExpr): boolean {
  if (a.tag === "Atomic" && b.tag === "Atomic") return termEq(a.term, b.term);
  if (a.tag === "Compound" && b.tag === "Compound") {
    if (a.elements.length !== b.elements.length) return false;
    return a.elements.every((e, i) => signedTermEq(e, b.elements[i]));
  }
  return false;
}

export type SignedTerm = {
  readonly sign: WildSign;
  readonly termExpr: TermExpr;
  readonly positions: readonly number[];
};

/** Extract the atomic Term from a SignedTerm.
 * For compound terms, returns a placeholder. */
export function term(st: SignedTerm): Term {
  return st.termExpr.tag === "Atomic"
    ? st.termExpr.term
    : { termName: "", complemented: false };
}

export type Statement = {
  readonly terms: readonly SignedTerm[];
};

export type Inference = {
  readonly premises: readonly Statement[];
  readonly conclusion: Statement;
};

export function flipSign(s: Sign): Sign {
  return s === Sign.Plus ? Sign.Minus : Sign.Plus;
}

export function isPositive(ws: WildSign): boolean {
  return ws.tag === "Fixed" && ws.sign === Sign.Plus;
}

export function isNegative(ws: WildSign): boolean {
  return ws.tag === "Fixed" && ws.sign === Sign.Minus;
}

export function isWild(ws: WildSign): boolean {
  return ws.tag === "Wild";
}

export function wildSignEq(a: WildSign, b: WildSign): boolean {
  if (a.tag === "Wild" && b.tag === "Wild") return true;
  if (a.tag === "Fixed" && b.tag === "Fixed") return a.sign === b.sign;
  return false;
}

export function signedTermEq(a: SignedTerm, b: SignedTerm): boolean {
  return (
    wildSignEq(a.sign, b.sign) &&
    termExprEq(a.termExpr, b.termExpr) &&
    a.positions.length === b.positions.length &&
    a.positions.every((p, i) => p === b.positions[i])
  );
}

// ---------------------------------------------------------------------------
// Relational lexicon
// ---------------------------------------------------------------------------

export type RelForms = {
  readonly relActive: string;
  readonly relPassive: string;
};

export type RelLexicon = ReadonlyMap<string, RelForms>;

export function defaultRelForms(name: string): RelForms {
  return { relActive: name + "-of", relPassive: name + "-by" };
}

export function lookupRelForms(lexicon: RelLexicon, name: string): RelForms {
  return lexicon.get(name) ?? defaultRelForms(name);
}
