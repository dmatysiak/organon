// TFL Pretty printer — ported from Organon.Tfl.Pretty

import {
  Sign,
  type Term,
  type WildSign,
  type SignedTerm,
  type Statement,
  type Inference,
  type RelLexicon,
  term,
  lookupRelForms,
} from "./types";
import type { Cancellation, CancelledPair, ValidationResult } from "./validity";

// ---------------------------------------------------------------------------
// Term and sign rendering
// ---------------------------------------------------------------------------

export function prettyTerm(t: Term): string {
  return t.complemented ? "non-" + t.termName : t.termName;
}

export function prettySign(s: Sign): string {
  return s === Sign.Plus ? "+" : "-";
}

export function prettyWildSign(ws: WildSign): string {
  return ws.tag === "Wild" ? "*" : prettySign(ws.sign);
}

export function prettyPositions(ps: readonly number[]): string {
  if (ps.length === 0) return "";
  return "<" + ps.join(",") + ">";
}

export function prettySignedTerm(st: SignedTerm): string {
  if (st.termExpr.tag === "Atomic") {
    return (
      prettyWildSign(st.sign) +
      " " +
      prettyTerm(st.termExpr.term) +
      prettyPositions(st.positions)
    );
  }
  // Compound
  return (
    prettyWildSign(st.sign) +
    " (" +
    st.termExpr.elements.map(prettyCompoundElement).join(" + ") +
    ")" +
    prettyPositions(st.positions)
  );
}

function prettyCompoundElement(st: SignedTerm): string {
  if (st.termExpr.tag === "Atomic") {
    return prettyTerm(st.termExpr.term);
  }
  return (
    "(" + st.termExpr.elements.map(prettyCompoundElement).join(" + ") + ")"
  );
}

// ---------------------------------------------------------------------------
// Statement rendering
// ---------------------------------------------------------------------------

export function prettyStatement(stmt: Statement): string {
  return stmt.terms.map(prettySignedTerm).join(" ");
}

export function prettyStatementEnglish(
  rl: RelLexicon,
  stmt: Statement,
): string {
  const ts = stmt.terms;
  // Relational: any term has positions
  if (ts.some((st) => st.positions.length > 0)) {
    const rel = ts.find((st) => st.positions.length >= 2);
    const nonRels = ts.filter((st) => st.positions.length < 2);
    if (rel && nonRels.length === 2) {
      const [subj, obj] = nonRels;
      const forms = lookupRelForms(rl, term(rel).termName);
      const subjPos = subj.positions[0] ?? 0;
      const relFirstPos = rel.positions[0] ?? 0;
      const isActive = subjPos === relFirstPos;
      const relForm = isActive ? forms.relActive : forms.relPassive;
      return (
        prettyQuantifier(subj.sign) +
        prettyTerm(term(subj)) +
        " is " +
        relForm +
        " " +
        prettyQuantifier(obj.sign) +
        prettyTerm(term(obj))
      );
    }
  }
  if (ts.length === 2) {
    const [a, b] = ts;
    // Compound predicate patterns
    if (a.termExpr.tag === "Atomic" && b.termExpr.tag === "Compound") {
      if (a.sign.tag === "Fixed" || a.sign.tag === "Wild") {
        const subjQ = prettyQuantifier(a.sign);
        const sName = prettyTerm(a.termExpr.term);
        if (b.sign.tag === "Fixed" && b.sign.sign === Sign.Plus) {
          return (
            subjQ + sName + " is " + prettyCompoundEnglish(b.termExpr.elements)
          );
        }
        if (b.sign.tag === "Fixed" && b.sign.sign === Sign.Minus) {
          return (
            subjQ +
            sName +
            " is not (both " +
            prettyCompoundEnglish(b.termExpr.elements) +
            ")"
          );
        }
      }
    }
    if (a.termExpr.tag === "Atomic" && b.termExpr.tag === "Atomic") {
      if (a.sign.tag === "Fixed" && b.sign.tag === "Fixed") {
        if (a.sign.sign === Sign.Minus && b.sign.sign === Sign.Plus) {
          return `every ${prettyTerm(a.termExpr.term)} is ${prettyTerm(b.termExpr.term)}`;
        }
        if (a.sign.sign === Sign.Minus && b.sign.sign === Sign.Minus) {
          return `no ${prettyTerm(a.termExpr.term)} is ${prettyTerm(b.termExpr.term)}`;
        }
        if (a.sign.sign === Sign.Plus && b.sign.sign === Sign.Plus) {
          return `some ${prettyTerm(a.termExpr.term)} is ${prettyTerm(b.termExpr.term)}`;
        }
        if (a.sign.sign === Sign.Plus && b.sign.sign === Sign.Minus) {
          return `some ${prettyTerm(a.termExpr.term)} is not ${prettyTerm(b.termExpr.term)}`;
        }
      }
      if (a.sign.tag === "Wild" && b.sign.tag === "Fixed") {
        if (b.sign.sign === Sign.Plus) {
          return `* ${prettyTerm(a.termExpr.term)} is ${prettyTerm(b.termExpr.term)}`;
        }
        return `* ${prettyTerm(a.termExpr.term)} is not ${prettyTerm(b.termExpr.term)}`;
      }
    }
  }
  // Fallback to algebraic
  return prettyStatement(stmt);
}

// ---------------------------------------------------------------------------
// Inference rendering
// ---------------------------------------------------------------------------

export function prettyInference(inf: Inference): string {
  const lines = inf.premises.map(prettyStatement);
  lines.push("\u2234 " + prettyStatement(inf.conclusion));
  return lines.join("\n");
}

export function prettyInferenceEnglish(rl: RelLexicon, inf: Inference): string {
  const lines = inf.premises.map((s) => prettyStatementEnglish(rl, s));
  lines.push("\u2234 " + prettyStatementEnglish(rl, inf.conclusion));
  return lines.join("\n");
}

function prettyQuantifier(ws: WildSign): string {
  if (ws.tag === "Fixed") {
    return ws.sign === Sign.Minus ? "every " : "some ";
  }
  return "* ";
}

function prettyCompoundEnglish(elements: readonly SignedTerm[]): string {
  return elements.map(prettyCompoundElementEnglish).join(" and ");
}

function prettyCompoundElementEnglish(st: SignedTerm): string {
  if (st.termExpr.tag === "Atomic") {
    return prettyTerm(st.termExpr.term);
  }
  return "(" + prettyCompoundEnglish(st.termExpr.elements) + ")";
}

// ---------------------------------------------------------------------------
// Cancellation rendering
// ---------------------------------------------------------------------------

export function prettyCancellation(cancel: Cancellation): string {
  const cancelLines =
    cancel.cancelled.length === 0
      ? ["No terms cancelled."]
      : ["Cancelled:", ...cancel.cancelled.map(prettyCancelledPair)];
  const remainLines =
    cancel.uncancelled.length === 0
      ? ["No uncancelled terms."]
      : [
          "Uncancelled:",
          ...cancel.uncancelled.map(
            ([i, st]) => `  ${prettySignedTerm(st)} (premise ${i + 1})`,
          ),
        ];
  return [...cancelLines, ...remainLines].join("\n");
}

function prettyCancelledPair(cp: CancelledPair): string {
  let tStr: string;
  if (cp.cancelledTermExpr.tag === "Atomic") {
    tStr = prettyTerm(cp.cancelledTermExpr.term);
  } else {
    tStr =
      "(" +
      cp.cancelledTermExpr.elements.map(prettyCompoundElement).join(" + ") +
      ")";
  }
  return `  ${tStr}${prettyPositions(cp.cancelledPositions)} (premise ${cp.premiseIndex1 + 1} \u2194 premise ${cp.premiseIndex2 + 1})`;
}

export function prettyValidationResult(result: ValidationResult): string {
  if (result.tag === "Valid") {
    return "Valid\n" + prettyCancellation(result.cancellation);
  }
  return (
    "Invalid\n" +
    prettyCancellation(result.cancellation) +
    "\nErrors:\n" +
    result.errors.map((e) => "  " + e).join("\n")
  );
}
