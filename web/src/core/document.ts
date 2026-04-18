// Document parser — ported from Organon.Syl.Document

import type { PropositionH } from "./hole";
import { ParserState, isNameChar, propositionHP } from "./parser";
import { Proposition, PropType, Tradition } from "./types";
import { ParseError } from "./parser";

// ---------------------------------------------------------------------------
// Source positions & Located
// ---------------------------------------------------------------------------

export type SrcPos = {
  readonly posLine: number;
  readonly posCol: number;
};

export type Located<T> = {
  readonly locStart: SrcPos;
  readonly locEnd: SrcPos;
  readonly locValue: T;
};

// ---------------------------------------------------------------------------
// Premise types
// ---------------------------------------------------------------------------

export type RefModifier = "conv" | "per_accidens";

export type Premise =
  | { readonly tag: "PremiseProp"; readonly prop: Proposition }
  | {
      readonly tag: "PremiseRef";
      readonly namespace: string | null;
      readonly name: string;
      readonly modifier: RefModifier | null;
    }
  | { readonly tag: "PremiseHole"; readonly propH: PropositionH };

// ---------------------------------------------------------------------------
// Proof block & Document
// ---------------------------------------------------------------------------

export type ProofBlock = {
  readonly proofName: Located<string>;
  readonly proofPremises: Located<Premise>[];
  readonly proofConclusion: Located<PropositionH>;
};

export type Document = {
  readonly docTradition: Located<Tradition> | null;
  readonly docOpens: Located<string>[];
  readonly docProofs: Located<ProofBlock>[];
};

// ---------------------------------------------------------------------------
// fromConcreteH
// ---------------------------------------------------------------------------

export function fromConcreteH(ph: PropositionH): Proposition | null {
  if (
    ph.tag === "PropH" &&
    ph.propTypeH.tag === "ConcretePT" &&
    ph.subject.tag === "ConcreteT" &&
    ph.predicate.tag === "ConcreteT"
  ) {
    return {
      propType: ph.propTypeH.propType,
      subject: ph.subject.term,
      predicate: ph.predicate.term,
    };
  }
  return null;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function getPos(p: ParserState): SrcPos {
  return { posLine: p.line, posCol: p.col };
}

function located<T>(
  p: ParserState,
  parse: (p: ParserState) => T | null,
): Located<T> | null {
  const start = getPos(p);
  const value = parse(p);
  if (value === null) return null;
  const end = getPos(p);
  return { locStart: start, locEnd: end, locValue: value };
}

// Consume an end-of-line (or EOF). Returns true if consumed.
function eol(p: ParserState): boolean {
  if (p.eof) return true;
  return p.newline();
}

// ---------------------------------------------------------------------------
// Premise parsing
// ---------------------------------------------------------------------------

function refModifierP(p: ParserState): RefModifier | null {
  if (p.symbol("conv")) return "conv";
  if (p.symbol("per_accidens")) return "per_accidens";
  return null;
}

function refP(p: ParserState): Premise | null {
  if (p.peek() !== "@") return null;
  p.advance(); // consume '@'
  const first = p.takeWhile1(isNameChar, "proof name");
  if (first === null) return null;
  if (!p.eof && p.peek() === ".") {
    p.advance(); // consume '.'
    const name = p.takeWhile1(isNameChar, "proof name");
    if (name === null) return null;
    const modifier = refModifierP(p);
    return { tag: "PremiseRef", namespace: first, name, modifier };
  }
  const modifier = refModifierP(p);
  return { tag: "PremiseRef", namespace: null, name: first, modifier };
}

function premiseP(p: ParserState): Premise | null {
  // Try reference first
  const ref = refP(p);
  if (ref !== null) return ref;

  // Otherwise parse as proposition (possibly with holes)
  const ph = propositionHP(p);
  if (ph === null) return null;

  const concrete = fromConcreteH(ph);
  if (concrete !== null) return { tag: "PremiseProp", prop: concrete };
  return { tag: "PremiseHole", propH: ph };
}

// ---------------------------------------------------------------------------
// Tradition directive
// ---------------------------------------------------------------------------

function traditionP(p: ParserState): Tradition | null {
  if (!p.symbol("tradition")) return null;
  if (p.symbol("Strict")) return Tradition.Strict;
  if (p.symbol("Traditional")) return Tradition.Traditional;
  if (p.symbol("Full")) return Tradition.Full;
  return null;
}

// ---------------------------------------------------------------------------
// Open directive
// ---------------------------------------------------------------------------

function openP(p: ParserState): string | null {
  if (!p.symbol("open")) return null;
  return p.takeWhile1(isNameChar, "namespace name");
}

// ---------------------------------------------------------------------------
// Proof block
// ---------------------------------------------------------------------------

function conclusionP(p: ParserState): Located<PropositionH> | null {
  if (!p.symbol("∴") && !p.symbol("therefore")) return null;
  return located(p, propositionHP);
}

function proofBlockP(p: ParserState): ProofBlock | null {
  if (!p.symbol("proof")) return null;

  const name = located(p, (pp) => pp.takeWhile1(isNameChar, "proof name"));
  if (name === null) return null;

  if (!eol(p)) return null;
  p.scn();

  const premises: Located<Premise>[] = [];
  while (true) {
    const s0 = p.save();
    const prem = located(p, premiseP);
    if (prem === null) {
      p.restore(s0);
      break;
    }
    // Premise must be followed by EOL (or EOF before conclusion)
    if (!eol(p)) {
      p.restore(s0);
      break;
    }
    p.scn();
    premises.push(prem);
  }

  const concl = conclusionP(p);
  if (concl === null) return null;

  return { proofName: name, proofPremises: premises, proofConclusion: concl };
}

// ---------------------------------------------------------------------------
// Document parser
// ---------------------------------------------------------------------------

export function parseDocument(input: string): Document | ParseError {
  const p = new ParserState(input);
  p.scn();

  // Optional tradition directive
  let docTradition: Located<Tradition> | null = null;
  {
    const s0 = p.save();
    const trad = located(p, traditionP);
    if (trad !== null) {
      if (eol(p)) {
        p.scn();
        docTradition = trad;
      } else {
        p.restore(s0);
      }
    } else {
      p.restore(s0);
    }
  }

  // Open directives
  const docOpens: Located<string>[] = [];
  while (true) {
    const s0 = p.save();
    const op = located(p, openP);
    if (op !== null && eol(p)) {
      p.scn();
      docOpens.push(op);
    } else {
      p.restore(s0);
      break;
    }
  }

  // Proof blocks
  const docProofs: Located<ProofBlock>[] = [];
  while (!p.eof) {
    p.scn();
    if (p.eof) break;
    const proof = located(p, proofBlockP);
    if (proof === null) {
      return p.error("Expected 'proof' block");
    }
    p.scn();
    docProofs.push(proof);
  }

  return { docTradition, docOpens, docProofs };
}
