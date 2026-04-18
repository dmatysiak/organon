// Parser — ported from Organon.Syl.Parser
// Hand-rolled recursive descent (replaces megaparsec).

import type { PropositionH, PropTypeH, SyllogismH, TermH } from "./hole";
import { PropType, Proposition, Syllogism, Term } from "./types";

// ---------------------------------------------------------------------------
// Parser state
// ---------------------------------------------------------------------------

export class ParseError {
  constructor(
    public readonly line: number,
    public readonly col: number,
    public readonly message: string,
  ) {}
}

class Parser {
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

  // Horizontal whitespace + line comments (no newlines)
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
        // Skip to end of line (but don't consume the newline)
        while (!this.eof && this.peek() !== "\n") this.advance();
      } else {
        break;
      }
    }
  }

  // Whitespace including newlines + line comments
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

  // Case-insensitive keyword match followed by horizontal whitespace.
  // Returns true if matched, false otherwise (and restores position).
  symbol(kw: string): boolean {
    const s = this.save();
    for (let i = 0; i < kw.length; i++) {
      if (this.eof || this.peek().toLowerCase() !== kw[i].toLowerCase()) {
        this.restore(s);
        return false;
      }
      this.advance();
    }
    // Keyword must not be followed by a name char (prevents "every" matching "everyX")
    if (!this.eof && isTermChar(this.peek()) && kw !== "∴") {
      this.restore(s);
      return false;
    }
    this.sc();
    return true;
  }

  // Take characters while predicate holds (at least 1).
  takeWhile1(pred: (c: string) => boolean, label: string): string | null {
    let result = "";
    while (!this.eof && pred(this.peek())) {
      result += this.advance();
    }
    if (result.length === 0) return null;
    this.sc();
    return result;
  }

  // Consume a newline (LF or CRLF). Returns true if consumed.
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

export function isTermChar(c: string): boolean {
  return isNameChar(c) && c !== "?" && c !== "@";
}

// ---------------------------------------------------------------------------
// Term parsers
// ---------------------------------------------------------------------------

function termP(p: Parser): Term | null {
  let complemented = false;
  if (p.symbol("non-")) {
    complemented = true;
  }
  const name = p.takeWhile1(isTermChar, "term");
  if (name === null) return null;
  return { termName: name, complemented };
}

function termHP(p: Parser): TermH | null {
  if (p.symbol("?")) {
    // Bare "?" is a hole only if not followed by a term char
    // (the symbol() call already checked word boundary)
    return { tag: "HoleT" };
  }
  const t = termP(p);
  if (t === null) return null;
  return { tag: "ConcreteT", term: t };
}

// ---------------------------------------------------------------------------
// Proposition parsers
// ---------------------------------------------------------------------------

function propositionP(p: Parser): Proposition | null {
  if (p.symbol("every")) {
    const s = termP(p);
    if (!s || !p.symbol("is")) return null;
    const pr = termP(p);
    if (!pr) return null;
    return { propType: PropType.A, subject: s, predicate: pr };
  }
  if (p.symbol("no")) {
    const s = termP(p);
    if (!s || !p.symbol("is")) return null;
    const pr = termP(p);
    if (!pr) return null;
    return { propType: PropType.E, subject: s, predicate: pr };
  }
  if (p.symbol("some")) {
    const s = termP(p);
    if (!s || !p.symbol("is")) return null;
    const neg = p.symbol("not");
    const pr = termP(p);
    if (!pr) return null;
    return {
      propType: neg ? PropType.O : PropType.I,
      subject: s,
      predicate: pr,
    };
  }
  return null;
}

function propositionHP(p: Parser): PropositionH | null {
  const s0 = p.save();

  // Try "every"
  if (p.symbol("every")) {
    const s = termHP(p);
    if (s && p.symbol("is")) {
      const pr = termHP(p);
      if (pr) {
        return {
          tag: "PropH",
          propTypeH: { tag: "ConcretePT", propType: PropType.A },
          subject: s,
          predicate: pr,
        };
      }
    }
    p.restore(s0);
  }

  // Try "no"
  if (p.symbol("no")) {
    const s = termHP(p);
    if (s && p.symbol("is")) {
      const pr = termHP(p);
      if (pr) {
        return {
          tag: "PropH",
          propTypeH: { tag: "ConcretePT", propType: PropType.E },
          subject: s,
          predicate: pr,
        };
      }
    }
    p.restore(s0);
  }

  // Try "some"
  if (p.symbol("some")) {
    const s = termHP(p);
    if (s && p.symbol("is")) {
      const neg = p.symbol("not");
      const pr = termHP(p);
      if (pr) {
        return {
          tag: "PropH",
          propTypeH: {
            tag: "ConcretePT",
            propType: neg ? PropType.O : PropType.I,
          },
          subject: s,
          predicate: pr,
        };
      }
    }
    p.restore(s0);
  }

  // Try quantifier hole: "? <term> is [not] <term>"
  {
    const sq = p.save();
    if (p.symbol("?")) {
      const s = termHP(p);
      if (s && p.symbol("is")) {
        p.symbol("not"); // optional
        const pr = termHP(p);
        if (pr) {
          return {
            tag: "PropH",
            propTypeH: { tag: "HolePT" },
            subject: s,
            predicate: pr,
          };
        }
      }
      p.restore(sq);
    }
  }

  // Whole-prop hole: bare "?"
  if (p.symbol("?")) {
    return { tag: "WholePropH" };
  }

  return null;
}

// ---------------------------------------------------------------------------
// Separator
// ---------------------------------------------------------------------------

function separator(p: Parser): boolean {
  if (p.symbol(";")) return true;
  if (p.newline()) {
    p.sc();
    return true;
  }
  return false;
}

// ---------------------------------------------------------------------------
// Syllogism parsers
// ---------------------------------------------------------------------------

function syllogismP(p: Parser): Syllogism | null {
  const maj = propositionP(p);
  if (!maj || !separator(p)) return null;
  const min = propositionP(p);
  if (!min || !separator(p)) return null;
  p.symbol("∴") || p.symbol("therefore");
  const concl = propositionP(p);
  if (!concl) return null;
  return { major: maj, minor: min, conclusion: concl };
}

function syllogismHP(p: Parser): SyllogismH | null {
  const maj = propositionHP(p);
  if (!maj || !separator(p)) return null;
  const min = propositionHP(p);
  if (!min || !separator(p)) return null;
  p.symbol("∴") || p.symbol("therefore");
  const concl = propositionHP(p);
  if (!concl) return null;
  return { majorH: maj, minorH: min, conclusionH: concl };
}

// ---------------------------------------------------------------------------
// Public parse functions
// ---------------------------------------------------------------------------

export function parseProposition(input: string): Proposition | ParseError {
  const p = new Parser(input);
  p.sc();
  const result = propositionP(p);
  if (!result) return p.error("Expected a proposition");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after proposition");
  return result;
}

export function parseSyllogism(input: string): Syllogism | ParseError {
  const p = new Parser(input);
  p.sc();
  const result = syllogismP(p);
  if (!result) return p.error("Expected a syllogism");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after syllogism");
  return result;
}

export function parsePropositionH(input: string): PropositionH | ParseError {
  const p = new Parser(input);
  p.sc();
  const result = propositionHP(p);
  if (!result) return p.error("Expected a proposition");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after proposition");
  return result;
}

export function parseSyllogismH(input: string): SyllogismH | ParseError {
  const p = new Parser(input);
  p.sc();
  const result = syllogismHP(p);
  if (!result) return p.error("Expected a syllogism");
  p.sc();
  if (!p.eof) return p.error("Unexpected input after syllogism");
  return result;
}

// Re-export Parser class for use by document.ts
export { Parser as ParserState };
export { termP, termHP, propositionP, propositionHP };
