// TFL hole types — ported from Organon.Tfl.Hole

import type { SignedTerm } from "./types";

export type SignedTermH =
  | { readonly tag: "ConcreteSTH"; readonly st: SignedTerm }
  | { readonly tag: "HoleSTH" };

export type StatementH =
  | { readonly tag: "StmtH"; readonly terms: readonly SignedTermH[] }
  | { readonly tag: "WholeStmtH" };

export type InferenceH = {
  readonly premises: readonly StatementH[];
  readonly conclusion: StatementH;
};
