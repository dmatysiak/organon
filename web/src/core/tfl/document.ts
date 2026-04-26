// TFL Document parser — ported from Organon.Tfl.Document

import type { StatementH, SignedTermH } from "./hole";
import {
  ParseError,
  ParserState,
  isNameChar,
  isTflTermChar,
  statementHP,
} from "./parser";
import type { Statement } from "./types";
import type { RelForms, RelLexicon } from "./types";

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

export type RefModifier = "conv" | "per-accidens" | "obv" | "contra";

export type Premise =
  | { readonly tag: "PremiseStmt"; readonly stmt: Statement }
  | {
      readonly tag: "PremiseRef";
      readonly namespace: string | null;
      readonly name: string;
      readonly modifier: RefModifier | null;
    }
  | { readonly tag: "PremiseHole"; readonly stmtH: StatementH };

// ---------------------------------------------------------------------------
// Proof block & Document
// ---------------------------------------------------------------------------

export type ProofBlock = {
  readonly proofName: Located<string>;
  readonly proofPremises: Located<Premise>[];
  readonly proofConclusion: Located<StatementH>;
};

export type Document = {
  readonly docOpens: Located<string>[];
  readonly docRelLexicon: RelLexicon;
  readonly docProofs: Located<ProofBlock>[];
};

// ---------------------------------------------------------------------------
// fromConcreteH
// ---------------------------------------------------------------------------

export function fromConcreteH(sh: StatementH): Statement | null {
  if (sh.tag === "WholeStmtH") return null;
  const terms: import("./types").SignedTerm[] = [];
  for (const sth of sh.terms) {
    if (sth.tag === "HoleSTH") return null;
    terms.push(sth.st);
  }
  return { terms };
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function getPos(p: ParserState): SrcPos {
  return { posLine: p.line, posCol: p.col };
}

function located<T>(p: ParserState, parse: () => T | null): Located<T> | null {
  const start = getPos(p);
  const value = parse();
  if (value === null) return null;
  const end = getPos(p);
  return { locStart: start, locEnd: end, locValue: value };
}

// ---------------------------------------------------------------------------
// Premise parsers
// ---------------------------------------------------------------------------

function refModifierP(p: ParserState): RefModifier | null {
  if (p.symbol("conv")) return "conv";
  if (p.symbol("per-accidens")) return "per-accidens";
  if (p.symbol("obv")) return "obv";
  if (p.symbol("contra")) return "contra";
  return null;
}

function refP(p: ParserState): Premise | null {
  const s = p.save();
  if (p.eof || p.peek() !== "@") {
    p.restore(s);
    return null;
  }
  p.advance(); // consume '@'
  const firstName = p.takeWhile1(isNameChar);
  if (firstName === null) {
    p.restore(s);
    return null;
  }
  p.sc();

  // Check for qualified name (namespace.name)
  if (!p.eof && p.peek() === ".") {
    p.advance();
    const secondName = p.takeWhile1(isNameChar);
    if (secondName === null) {
      p.restore(s);
      return null;
    }
    p.sc();
    const modifier = refModifierP(p);
    return {
      tag: "PremiseRef",
      namespace: firstName,
      name: secondName,
      modifier,
    };
  }

  const modifier = refModifierP(p);
  return { tag: "PremiseRef", namespace: null, name: firstName, modifier };
}

function stmtPremiseP(p: ParserState): Premise | null {
  const sh = statementHP(p);
  if (sh === null) return null;
  const concrete = fromConcreteH(sh);
  if (concrete !== null) return { tag: "PremiseStmt", stmt: concrete };
  return { tag: "PremiseHole", stmtH: sh };
}

function premiseP(p: ParserState): Premise | null {
  const s = p.save();
  const ref = refP(p);
  if (ref !== null) return ref;
  p.restore(s);
  return stmtPremiseP(p);
}

// ---------------------------------------------------------------------------
// Proof block parser
// ---------------------------------------------------------------------------

function proofBlockP(p: ParserState): ProofBlock | null {
  if (!p.symbol("proof")) return null;
  const nameStart = getPos(p);
  const name = p.takeWhile1(isNameChar);
  if (name === null) return null;
  p.sc();
  const nameEnd = getPos(p);
  const proofName: Located<string> = {
    locStart: nameStart,
    locEnd: nameEnd,
    locValue: name,
  };

  // Expect newline after proof name
  if (!p.newline()) return null;
  p.scn();

  // Parse premises (lines before ∴)
  const premises: Located<Premise>[] = [];
  while (true) {
    const s = p.save();
    // Check if we've hit the conclusion marker
    if (p.symbol("∴") || p.symbol("therefore")) {
      p.restore(s);
      break;
    }
    p.restore(s);
    if (p.eof) break;

    const lp = located(p, () => premiseP(p));
    if (lp === null) break;
    premises.push(lp);
    p.newline();
    p.scn();
  }

  // Parse conclusion: ∴ <statementH>
  if (!p.symbol("∴") && !p.symbol("therefore")) return null;
  const conclStart = getPos(p);
  const conclSH = statementHP(p);
  if (conclSH === null) return null;
  const conclEnd = getPos(p);
  const proofConclusion: Located<StatementH> = {
    locStart: conclStart,
    locEnd: conclEnd,
    locValue: conclSH,
  };

  return { proofName, proofPremises: premises, proofConclusion };
}

// ---------------------------------------------------------------------------
// Open directive
// ---------------------------------------------------------------------------

function openP(p: ParserState): string | null {
  if (!p.symbol("open")) return null;
  const name = p.takeWhile1(isNameChar);
  if (name === null) return null;
  p.sc();
  return name;
}

// ---------------------------------------------------------------------------
// Rel directive parser
// ---------------------------------------------------------------------------

function quotedStringP(p: ParserState): string | null {
  if (p.eof || p.peek() !== '"') return null;
  p.advance();
  let result = "";
  while (!p.eof && p.peek() !== '"') {
    result += p.peek();
    p.advance();
  }
  if (p.eof) return null;
  p.advance(); // closing quote
  p.sc();
  return result;
}

function relP(p: ParserState): [string, RelForms] | null {
  const s = p.save();
  if (!p.symbol("rel")) {
    p.restore(s);
    return null;
  }
  const name = p.takeWhile1(isTflTermChar);
  if (name === null) {
    p.restore(s);
    return null;
  }
  p.sc();
  const active = quotedStringP(p);
  if (active === null) {
    p.restore(s);
    return null;
  }
  const passive = quotedStringP(p);
  if (passive === null) {
    p.restore(s);
    return null;
  }
  return [name, { relActive: active, relPassive: passive }];
}

// ---------------------------------------------------------------------------
// Document parser
// ---------------------------------------------------------------------------

function documentP(p: ParserState): Document {
  const opens: Located<string>[] = [];
  const rels: [string, RelForms][] = [];
  const proofs: Located<ProofBlock>[] = [];

  p.scn();

  // Parse open directives
  while (true) {
    const s = p.save();
    const lo = located(p, () => openP(p));
    if (lo === null) {
      p.restore(s);
      break;
    }
    opens.push(lo);
    p.newline();
    p.scn();
  }

  // Parse rel directives
  while (true) {
    const s = p.save();
    const r = relP(p);
    if (r === null) {
      p.restore(s);
      break;
    }
    rels.push(r);
    p.newline();
    p.scn();
  }

  // Parse proof blocks
  while (!p.eof) {
    const lp = located(p, () => proofBlockP(p));
    if (lp === null) break;
    proofs.push(lp);
    p.scn();
  }

  return {
    docOpens: opens,
    docRelLexicon: new Map(rels),
    docProofs: proofs,
  };
}

export function parseDocument(input: string): Document | ParseError {
  const p = new ParserState(input);
  const doc = documentP(p);
  if (!p.eof) {
    return new ParseError(
      p.line,
      p.col,
      `Unexpected input at line ${p.line}, column ${p.col}`,
    );
  }
  return doc;
}
