// TFL Parser — ported from Organon.Tfl.Parser
// Hand-rolled recursive descent (replaces megaparsec).

import type { SignedTermH, StatementH, InferenceH } from "./hole";
import {
  Sign,
  Wild,
  Fixed,
  Atomic,
  Compound,
  type WildSign,
  type Term,
  type TermExpr,
  type SignedTerm,
  type Statement,
  type Inference,
} from "./types";

// ---------------------------------------------------------------------------
// Parse error
// ---------------------------------------------------------------------------

export class ParseError {
  constructor(
    public readonly line: number,
    public readonly col: number,
    public readonly message: string,
  ) {}
}

// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------

export class ParserState {
  pos = 0;
  line = 1;
  col = 1;

  constructor(public readonly input: string) {}

  get eof(): boolean {
    return this.pos >= this.input.length;
  }

  peek(): string {
    return this.input[this.pos] ?? "";
  }

  advance(): string {
    const ch = this.input[this.pos++];
    if (ch === "\n") {
      this.line++;
      this.col = 1;
    } else {
      this.col++;
    }
    return ch;
  }

  save(): { pos: number; line: number; col: number } {
    return { pos: this.pos, line: this.line, col: this.col };
  }

  restore(s: { pos: number; line: number; col: number }): void {
    this.pos = s.pos;
    this.line = s.line;
    this.col = s.col;
  }

  error(msg: string): ParseError {
    return new ParseError(this.line, this.col, msg);
  }

  /** Horizontal whitespace + line comments (no newlines). */
  sc(): void {
    while (!this.eof) {
      const ch = this.peek();
      if (ch === " " || ch === "\t") {
        this.advance();
      } else if (
        ch === "-" &&
        this.pos + 1 < this.input.length &&
        this.input[this.pos + 1] === "-"
      ) {
        while (!this.eof && this.peek() !== "\n") this.advance();
      } else {
        break;
      }
    }
  }

  /** Whitespace including newlines + line comments. */
  scn(): void {
    while (!this.eof) {
      const ch = this.peek();
      if (ch === " " || ch === "\t" || ch === "\n" || ch === "\r") {
        this.advance();
      } else if (
        ch === "-" &&
        this.pos + 1 < this.input.length &&
        this.input[this.pos + 1] === "-"
      ) {
        while (!this.eof && this.peek() !== "\n") this.advance();
      } else {
        break;
      }
    }
  }

  /**
   * Case-insensitive keyword match followed by horizontal whitespace.
   * Returns true if matched, false otherwise (restores position).
   */
  symbol(kw: string): boolean {
    const s = this.save();
    for (let i = 0; i < kw.length; i++) {
      if (this.eof || this.peek().toLowerCase() !== kw[i].toLowerCase()) {
        this.restore(s);
        return false;
      }
      this.advance();
    }
    // Keyword must not be followed by a term char (prevents "every" matching "everyX")
    if (!this.eof && isTflTermChar(this.peek()) && kw !== "∴") {
      this.restore(s);
      return false;
    }
    this.sc();
    return true;
  }

  /** Take characters while predicate holds (at least 1). Returns null on failure. */
  takeWhile1(pred: (c: string) => boolean): string | null {
    let result = "";
    while (!this.eof && pred(this.peek())) {
      result += this.advance();
    }
    return result.length === 0 ? null : result;
  }

  /** Consume horizontal whitespace only (no newlines). */
  hspace(): void {
    while (!this.eof) {
      const ch = this.peek();
      if (ch === " " || ch === "\t") {
        this.advance();
      } else {
        break;
      }
    }
  }

  /** Consume a newline (LF or CRLF). Returns true if consumed. */
  newline(): boolean {
    if (this.peek() === "\r") {
      this.advance();
      if (this.peek() === "\n") this.advance();
      return true;
    }
    if (this.peek() === "\n") {
      this.advance();
      return true;
    }
    return false;
  }
}

// ---------------------------------------------------------------------------
// Character predicates
// ---------------------------------------------------------------------------

export function isNameChar(c: string): boolean {
  return (
    c !== " " &&
    c !== "\t" &&
    c !== ";" &&
    c !== "." &&
    c !== "\n" &&
    c !== "\r"
  );
}

export function isTflTermChar(c: string): boolean {
  return (
    isNameChar(c) &&
    c !== "+" &&
    c !== "-" &&
    c !== "*" &&
    c !== "?" &&
    c !== "@" &&
    c !== "<" &&
    c !== ">" &&
    c !== "(" &&
    c !== ")"
  );
}

// ---------------------------------------------------------------------------
// Term parsers
// ---------------------------------------------------------------------------

function tflTermP(p: ParserState): Term | null {
  const s = p.save();
  let complemented = false;
  // Check for "non-" prefix (glued in algebraic, lexeme in English)
  const nons = p.save();
  if (
    !p.eof &&
    p.peek() === "n" &&
    p.input.substring(p.pos, p.pos + 4) === "non-"
  ) {
    p.pos += 4;
    p.line = nons.line;
    p.col = nons.col + 4;
    complemented = true;
  }
  const name = p.takeWhile1(isTflTermChar);
  if (name === null) {
    p.restore(s);
    return null;
  }
  return { termName: name, complemented };
}

function englishTermP(p: ParserState): Term | null {
  let complemented = false;
  if (p.symbol("non-")) {
    complemented = true;
  }
  const name = p.takeWhile1(isTflTermChar);
  if (name === null) return null;
  p.sc();
  return { termName: name, complemented };
}

/** Parse optional positional subscripts: <1>, <1,2>, etc. */
function positionsP(p: ParserState): number[] {
  if (p.eof || p.peek() !== "<") return [];
  p.advance(); // consume '<'
  const nums: number[] = [];
  const first = parseDecimal(p);
  if (first === null) return [];
  nums.push(first);
  while (!p.eof && p.peek() === ",") {
    p.advance(); // consume ','
    const n = parseDecimal(p);
    if (n === null) break;
    nums.push(n);
  }
  if (!p.eof && p.peek() === ">") {
    p.advance(); // consume '>'
  }
  return nums;
}

function parseDecimal(p: ParserState): number | null {
  let digits = "";
  while (!p.eof && p.peek() >= "0" && p.peek() <= "9") {
    digits += p.advance();
  }
  return digits.length === 0 ? null : parseInt(digits, 10);
}

// ---------------------------------------------------------------------------
// Algebraic syntax
// ---------------------------------------------------------------------------

function signCharP(p: ParserState): WildSign | null {
  const ch = p.peek();
  if (ch === "+") {
    p.advance();
    return Fixed(Sign.Plus);
  }
  if (ch === "-") {
    p.advance();
    return Fixed(Sign.Minus);
  }
  if (ch === "*") {
    p.advance();
    return Wild;
  }
  return null;
}

function algebraicSignedTermP(p: ParserState): SignedTerm | null {
  const s = p.save();
  const sign = signCharP(p);
  if (sign === null) {
    p.restore(s);
    return null;
  }
  p.hspace();
  // Try compound first, then atomic
  const compound = compoundTermExprP(p);
  let te: TermExpr;
  if (compound !== null) {
    te = compound;
  } else {
    const t = tflTermP(p);
    if (t === null) {
      p.restore(s);
      return null;
    }
    te = Atomic(t);
  }
  const positions = positionsP(p);
  p.sc();
  return { sign, termExpr: te, positions };
}

/** Parse a compound term expression: (A + B) */
function compoundTermExprP(p: ParserState): TermExpr | null {
  const s = p.save();
  if (p.eof || p.peek() !== "(") {
    return null;
  }
  p.advance(); // consume '('
  p.sc();
  const first = compoundElementP(p);
  if (first === null) {
    p.restore(s);
    return null;
  }
  const rest: SignedTerm[] = [];
  while (true) {
    const s2 = p.save();
    p.sc();
    if (p.eof || p.peek() !== "+") {
      p.restore(s2);
      break;
    }
    p.advance(); // consume '+'
    p.sc();
    const elem = compoundElementP(p);
    if (elem === null) {
      p.restore(s);
      return null;
    }
    rest.push(elem);
  }
  if (rest.length === 0) {
    // Not a compound — need at least 2 elements
    p.restore(s);
    return null;
  }
  p.sc();
  if (p.eof || p.peek() !== ")") {
    p.restore(s);
    return null;
  }
  p.advance(); // consume ')'
  return Compound([first, ...rest]);
}

/** Parse a single element inside a compound: nested compound or atomic term. */
function compoundElementP(p: ParserState): SignedTerm | null {
  // Try nested compound
  const nested = compoundTermExprP(p);
  if (nested !== null) {
    return { sign: Fixed(Sign.Plus), termExpr: nested, positions: [] };
  }
  // Atomic term (implicit + sign)
  const t = tflTermP(p);
  if (t === null) return null;
  return { sign: Fixed(Sign.Plus), termExpr: Atomic(t), positions: [] };
}

function algebraicStatementP(p: ParserState): Statement | null {
  const terms: SignedTerm[] = [];
  let st = algebraicSignedTermP(p);
  while (st !== null) {
    terms.push(st);
    st = algebraicSignedTermP(p);
  }
  return terms.length === 0 ? null : { terms };
}

// ---------------------------------------------------------------------------
// Separator
// ---------------------------------------------------------------------------

function separator(p: ParserState): boolean {
  if (p.symbol(";")) return true;
  if (p.newline()) {
    p.sc();
    return true;
  }
  return false;
}

// ---------------------------------------------------------------------------
// Regimented English syntax
// ---------------------------------------------------------------------------

/** Parse a quantified term: every/no/some/* followed by a term. */
function quantifiedTermP(
  p: ParserState,
): { sign: WildSign; term: Term } | null {
  const s = p.save();
  if (p.symbol("every") || p.symbol("no")) {
    const t = englishTermP(p);
    if (t !== null) return { sign: Fixed(Sign.Minus), term: t };
    p.restore(s);
    return null;
  }
  p.restore(s);
  if (p.symbol("some")) {
    const t = englishTermP(p);
    if (t !== null) return { sign: Fixed(Sign.Plus), term: t };
    p.restore(s);
    return null;
  }
  p.restore(s);
  if (!p.eof && p.peek() === "*") {
    p.advance();
    p.sc();
    const t = englishTermP(p);
    if (t !== null) return { sign: Wild, term: t };
    p.restore(s);
    return null;
  }
  return null;
}

/**
 * Try to parse a relational predicate after "is": Name-of (active)
 * or Name-by (passive), followed by a quantified object term.
 * Returns null if no relational suffix found.
 */
function relationalAfterIs(
  p: ParserState,
  subjSign: WildSign,
  subj: Term,
): Statement | null {
  const s = p.save();
  // Read the relation name (stops before '-')
  const relName = p.takeWhile1(isTflTermChar);
  if (relName === null) {
    p.restore(s);
    return null;
  }
  // Check for -of or -by suffix
  const remaining = p.input.substring(p.pos);
  if (remaining.startsWith("-of")) {
    p.pos += 3;
    p.col += 3;
    p.sc();
    const obj = quantifiedTermP(p);
    if (obj !== null) {
      return {
        terms: [
          { sign: subjSign, termExpr: Atomic(subj), positions: [1] },
          {
            sign: Fixed(Sign.Plus),
            termExpr: Atomic({ termName: relName, complemented: false }),
            positions: [1, 2],
          },
          { sign: obj.sign, termExpr: Atomic(obj.term), positions: [2] },
        ],
      };
    }
  } else if (remaining.startsWith("-by")) {
    p.pos += 3;
    p.col += 3;
    p.sc();
    const obj = quantifiedTermP(p);
    if (obj !== null) {
      // Passive: subject pos 2, object pos 1
      return {
        terms: [
          { sign: subjSign, termExpr: Atomic(subj), positions: [2] },
          {
            sign: Fixed(Sign.Plus),
            termExpr: Atomic({ termName: relName, complemented: false }),
            positions: [1, 2],
          },
          { sign: obj.sign, termExpr: Atomic(obj.term), positions: [1] },
        ],
      };
    }
  }
  p.restore(s);
  return null;
}

type PredicateResult = { pSign: Sign; pExpr: TermExpr };

/** Parse an English predicate: compound conjunction or atomic term. */
function englishPredicateP(
  p: ParserState,
  defaultSign: Sign,
): PredicateResult | null {
  // Try compound: "X and Y"
  const s = p.save();
  const first = englishTermP(p);
  if (first !== null && p.symbol("and")) {
    const terms: Term[] = [first];
    const t2 = englishTermP(p);
    if (t2 !== null) {
      terms.push(t2);
      while (p.symbol("and")) {
        const tn = englishTermP(p);
        if (tn === null) break;
        terms.push(tn);
      }
      const elems: SignedTerm[] = terms.map((t) => ({
        sign: Fixed(Sign.Plus),
        termExpr: Atomic(t),
        positions: [],
      }));
      return { pSign: Sign.Plus, pExpr: Compound(elems) };
    }
  }
  p.restore(s);
  // Atomic fallback
  const pred = englishTermP(p);
  if (pred === null) return null;
  return { pSign: defaultSign, pExpr: Atomic(pred) };
}

/** Parse a negated predicate: "(both non-X and non-Y)" or atomic. */
function englishNegatedPredicateP(p: ParserState): PredicateResult | null {
  const s = p.save();
  if (p.symbol("(") && p.symbol("both")) {
    const terms: Term[] = [];
    const first = englishTermP(p);
    if (first !== null) {
      terms.push(first);
      while (p.symbol("and")) {
        const tn = englishTermP(p);
        if (tn === null) break;
        terms.push(tn);
      }
      if (p.symbol(")") && terms.length >= 2) {
        const elems: SignedTerm[] = terms.map((t) => ({
          sign: Fixed(Sign.Plus),
          termExpr: Atomic(t),
          positions: [],
        }));
        return { pSign: Sign.Minus, pExpr: Compound(elems) };
      }
    }
  }
  p.restore(s);
  // Atomic fallback
  const pred = englishTermP(p);
  if (pred === null) return null;
  return { pSign: Sign.Minus, pExpr: Atomic(pred) };
}

function englishEvery(p: ParserState): Statement | null {
  const s = p.save();
  if (!p.symbol("every")) {
    p.restore(s);
    return null;
  }
  const subj = englishTermP(p);
  if (subj === null) {
    p.restore(s);
    return null;
  }
  if (!p.symbol("is")) {
    p.restore(s);
    return null;
  }
  const rel = relationalAfterIs(p, Fixed(Sign.Minus), subj);
  if (rel !== null) return rel;
  const predResult = englishPredicateP(p, Sign.Plus);
  if (predResult === null) {
    p.restore(s);
    return null;
  }
  return {
    terms: [
      { sign: Fixed(Sign.Minus), termExpr: Atomic(subj), positions: [] },
      {
        sign: Fixed(predResult.pSign),
        termExpr: predResult.pExpr,
        positions: [],
      },
    ],
  };
}

function englishNo(p: ParserState): Statement | null {
  const s = p.save();
  if (!p.symbol("no")) {
    p.restore(s);
    return null;
  }
  const subj = englishTermP(p);
  if (subj === null) {
    p.restore(s);
    return null;
  }
  if (!p.symbol("is")) {
    p.restore(s);
    return null;
  }
  const rel = relationalAfterIs(p, Fixed(Sign.Minus), subj);
  if (rel !== null) return rel;
  const predResult = englishPredicateP(p, Sign.Minus);
  if (predResult === null) {
    p.restore(s);
    return null;
  }
  return {
    terms: [
      { sign: Fixed(Sign.Minus), termExpr: Atomic(subj), positions: [] },
      {
        sign: Fixed(predResult.pSign),
        termExpr: predResult.pExpr,
        positions: [],
      },
    ],
  };
}

function englishSome(p: ParserState): Statement | null {
  const s = p.save();
  if (!p.symbol("some")) {
    p.restore(s);
    return null;
  }
  const subj = englishTermP(p);
  if (subj === null) {
    p.restore(s);
    return null;
  }
  if (!p.symbol("is")) {
    p.restore(s);
    return null;
  }
  const rel = relationalAfterIs(p, Fixed(Sign.Plus), subj);
  if (rel !== null) return rel;
  const neg = p.symbol("not");
  const predResult = neg
    ? englishNegatedPredicateP(p)
    : englishPredicateP(p, Sign.Plus);
  if (predResult === null) {
    p.restore(s);
    return null;
  }
  return {
    terms: [
      { sign: Fixed(Sign.Plus), termExpr: Atomic(subj), positions: [] },
      {
        sign: Fixed(predResult.pSign),
        termExpr: predResult.pExpr,
        positions: [],
      },
    ],
  };
}

function englishWild(p: ParserState): Statement | null {
  const s = p.save();
  if (p.eof || p.peek() !== "*") {
    p.restore(s);
    return null;
  }
  p.advance();
  p.sc();
  const subj = englishTermP(p);
  if (subj === null) {
    p.restore(s);
    return null;
  }
  if (!p.symbol("is")) {
    p.restore(s);
    return null;
  }
  const rel = relationalAfterIs(p, Wild, subj);
  if (rel !== null) return rel;
  const neg = p.symbol("not");
  const predResult = neg
    ? englishNegatedPredicateP(p)
    : englishPredicateP(p, Sign.Plus);
  if (predResult === null) {
    p.restore(s);
    return null;
  }
  return {
    terms: [
      { sign: Wild, termExpr: Atomic(subj), positions: [] },
      {
        sign: Fixed(predResult.pSign),
        termExpr: predResult.pExpr,
        positions: [],
      },
    ],
  };
}

function englishStatementP(p: ParserState): Statement | null {
  const s = p.save();
  let stmt: Statement | null;
  stmt = englishEvery(p);
  if (stmt !== null) return stmt;
  p.restore(s);
  stmt = englishNo(p);
  if (stmt !== null) return stmt;
  p.restore(s);
  stmt = englishSome(p);
  if (stmt !== null) return stmt;
  p.restore(s);
  stmt = englishWild(p);
  if (stmt !== null) return stmt;
  p.restore(s);
  return null;
}

// ---------------------------------------------------------------------------
// Unified parsers
// ---------------------------------------------------------------------------

function statementP(p: ParserState): Statement | null {
  // Try English first (unambiguous keywords), then algebraic
  const s = p.save();
  const eng = englishStatementP(p);
  if (eng !== null) return eng;
  p.restore(s);
  return algebraicStatementP(p);
}

function statementsWithConclusionP(
  p: ParserState,
  stmtParser: (p: ParserState) => Statement | null,
): Statement[] | null {
  const hasConcMarker = p.symbol("∴") || p.symbol("therefore");
  const stmt = stmtParser(p);
  if (stmt === null) return null;
  if (hasConcMarker) return [stmt];
  const s = p.save();
  if (separator(p)) {
    const rest = statementsWithConclusionP(p, stmtParser);
    if (rest !== null) return [stmt, ...rest];
  }
  p.restore(s);
  return [stmt];
}

function algebraicInferenceP(p: ParserState): Inference | null {
  const first = algebraicStatementP(p);
  if (first === null) return null;
  if (!separator(p)) return null;
  const rest = statementsWithConclusionP(p, algebraicStatementP);
  if (rest === null) return null;
  const all = [first, ...rest];
  return {
    premises: all.slice(0, -1),
    conclusion: all[all.length - 1],
  };
}

function englishInferenceP(p: ParserState): Inference | null {
  const first = englishStatementP(p);
  if (first === null) return null;
  if (!separator(p)) return null;
  const rest = statementsWithConclusionP(p, englishStatementP);
  if (rest === null) return null;
  const all = [first, ...rest];
  return {
    premises: all.slice(0, -1),
    conclusion: all[all.length - 1],
  };
}

function inferenceP(p: ParserState): Inference | null {
  const s = p.save();
  const eng = englishInferenceP(p);
  if (eng !== null) return eng;
  p.restore(s);
  return algebraicInferenceP(p);
}

// ---------------------------------------------------------------------------
// Hole-aware parsers
// ---------------------------------------------------------------------------

function signedTermHP(p: ParserState): SignedTermH | null {
  if (p.symbol("?")) {
    return { tag: "HoleSTH" };
  }
  const st = algebraicSignedTermP(p);
  if (st === null) return null;
  return { tag: "ConcreteSTH", st };
}

export function statementHP(p: ParserState): StatementH | null {
  // Whole-statement hole
  const s = p.save();
  if (p.symbol("?")) {
    // Make sure '?' is not followed by a term char
    if (p.eof || !isTflTermChar(p.peek())) {
      return { tag: "WholeStmtH" };
    }
    p.restore(s);
  }
  // Try English (concrete only)
  const eng = englishStatementP(p);
  if (eng !== null) {
    return {
      tag: "StmtH",
      terms: eng.terms.map((st) => ({ tag: "ConcreteSTH" as const, st })),
    };
  }
  p.restore(s);
  // Algebraic with possible holes
  const terms: SignedTermH[] = [];
  let th = signedTermHP(p);
  while (th !== null) {
    terms.push(th);
    th = signedTermHP(p);
  }
  return terms.length === 0 ? null : { tag: "StmtH", terms };
}

function holeStatementsWithConclusionP(p: ParserState): StatementH[] | null {
  const hasConcMarker = p.symbol("∴") || p.symbol("therefore");
  const stmt = statementHP(p);
  if (stmt === null) return null;
  if (hasConcMarker) return [stmt];
  const s = p.save();
  if (separator(p)) {
    const rest = holeStatementsWithConclusionP(p);
    if (rest !== null) return [stmt, ...rest];
  }
  p.restore(s);
  return [stmt];
}

export function inferenceHP(p: ParserState): InferenceH | null {
  const first = statementHP(p);
  if (first === null) return null;
  if (!separator(p)) return null;
  const rest = holeStatementsWithConclusionP(p);
  if (rest === null) return null;
  const all = [first, ...rest];
  return {
    premises: all.slice(0, -1),
    conclusion: all[all.length - 1],
  };
}

// ---------------------------------------------------------------------------
// Top-level parse functions
// ---------------------------------------------------------------------------

export function parseStatement(input: string): Statement | ParseError {
  const p = new ParserState(input);
  p.sc();
  const stmt = statementP(p);
  if (stmt === null) return p.error("Expected a statement");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after statement");
  return stmt;
}

export function parseInference(input: string): Inference | ParseError {
  const p = new ParserState(input);
  p.sc();
  const inf = inferenceP(p);
  if (inf === null) return p.error("Expected an inference");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after inference");
  return inf;
}

export function parseStatementH(input: string): StatementH | ParseError {
  const p = new ParserState(input);
  p.sc();
  const stmt = statementHP(p);
  if (stmt === null)
    return p.error("Expected a statement (with possible holes)");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after statement");
  return stmt;
}

export function parseInferenceH(input: string): InferenceH | ParseError {
  const p = new ParserState(input);
  p.sc();
  const inf = inferenceHP(p);
  if (inf === null)
    return p.error("Expected an inference (with possible holes)");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after inference");
  return inf;
}
