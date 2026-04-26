// TFL Validity — ported from Organon.Tfl.Validity

import {
  type WildSign,
  type SignedTerm,
  type Statement,
  type Inference,
  type TermExpr,
  Sign,
  signedTermEq,
} from "./types";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export type CancelledPair = {
  readonly cancelledTermExpr: TermExpr;
  readonly cancelledPositions: readonly number[];
  readonly premiseIndex1: number;
  readonly premiseIndex2: number;
};

export type Cancellation = {
  readonly cancelled: readonly CancelledPair[];
  readonly uncancelled: readonly [number, SignedTerm][];
};

export type ValidationResult =
  | { readonly tag: "Valid"; readonly cancellation: Cancellation }
  | {
      readonly tag: "Invalid";
      readonly cancellation: Cancellation;
      readonly errors: readonly string[];
    };

// ---------------------------------------------------------------------------
// Cancellation
// ---------------------------------------------------------------------------

type Indexed = [number, SignedTerm];

export function cancellation(stmts: readonly Statement[]): Cancellation {
  const indexed: Indexed[] = [];
  for (let i = 0; i < stmts.length; i++) {
    for (const st of stmts[i].terms) {
      indexed.push([i, st]);
    }
  }

  const grouped = groupByTermAndPositions(indexed);
  const allPairs: CancelledPair[] = [];
  const allRemaining: Indexed[] = [];

  for (const group of grouped) {
    const [pairs, remaining] = cancelGroup(group);
    allPairs.push(...pairs);
    allRemaining.push(...remaining);
  }

  return { cancelled: allPairs, uncancelled: allRemaining };
}

/** Group indexed signed terms by (termExpr, positions). */
function groupByTermAndPositions(entries: Indexed[]): Indexed[][] {
  const keyOf = (e: Indexed): string => {
    const st = e[1];
    const posKey = st.positions.join(",");
    return `${termExprKey(st.termExpr)}|${posKey}`;
  };

  const map = new Map<string, Indexed[]>();
  for (const e of entries) {
    const k = keyOf(e);
    const arr = map.get(k);
    if (arr) {
      arr.push(e);
    } else {
      map.set(k, [e]);
    }
  }
  return Array.from(map.values());
}

function termExprKey(te: TermExpr): string {
  if (te.tag === "Atomic") {
    return `A:${te.term.termName}|${te.term.complemented}`;
  }
  return `C:(${te.elements.map((st) => termExprKey(st.termExpr)).join("+")})`;
}

/** Greedily pair opposite-sign terms from different premises. */
function cancelGroup(entries: Indexed[]): [CancelledPair[], Indexed[]] {
  const pairs: CancelledPair[] = [];
  const pool = [...entries];
  const remaining: Indexed[] = [];

  for (let i = 0; i < pool.length; i++) {
    const e = pool[i];
    if (e === null) continue;
    let found = false;
    for (let j = i + 1; j < pool.length; j++) {
      const c = pool[j];
      if (c === null) continue;
      if (e[0] !== c[0] && signsOppose(e[1].sign, c[1].sign)) {
        pairs.push({
          cancelledTermExpr: e[1].termExpr,
          cancelledPositions: [...e[1].positions],
          premiseIndex1: e[0],
          premiseIndex2: c[0],
        });
        (pool as (Indexed | null)[])[j] = null;
        found = true;
        break;
      }
    }
    if (!found) {
      remaining.push(e);
    }
  }

  return [pairs, remaining];
}

function signsOppose(a: WildSign, b: WildSign): boolean {
  if (a.tag === "Fixed" && b.tag === "Fixed") {
    return a.sign !== b.sign;
  }
  // Wild opposes anything
  if (a.tag === "Wild" || b.tag === "Wild") return true;
  return false;
}

// ---------------------------------------------------------------------------
// Validation
// ---------------------------------------------------------------------------

export function validate(inf: Inference): ValidationResult {
  const cancel = cancellation(inf.premises);
  const uncancelledTerms = cancel.uncancelled.map(([_, st]) => st);
  const conclTerms = inf.conclusion.terms;
  const errors = checkConclusion(uncancelledTerms, [...conclTerms]);
  if (errors.length === 0) {
    return { tag: "Valid", cancellation: cancel };
  }
  return { tag: "Invalid", cancellation: cancel, errors };
}

function checkConclusion(
  uncancelled: readonly SignedTerm[],
  conclusion: readonly SignedTerm[],
): string[] {
  const sortST = (arr: readonly SignedTerm[]) =>
    [...arr].sort((a, b) => {
      const cmp = termKey(a).localeCompare(termKey(b));
      return cmp;
    });

  const sortedU = sortST(uncancelled);
  const sortedC = sortST(conclusion);

  const [missing, extra] = diffTerms(sortedU, sortedC);

  const errors: string[] = [];
  for (const st of missing) {
    errors.push(
      `Uncancelled premise term ${prettySignedTermBrief(st)} is missing from the conclusion`,
    );
  }
  for (const st of extra) {
    errors.push(
      `Conclusion term ${prettySignedTermBrief(st)} is not found among uncancelled premise terms`,
    );
  }
  return errors;
}

function termKey(st: SignedTerm): string {
  const posKey = st.positions.join(",");
  const signKey =
    st.sign.tag === "Wild" ? "*" : st.sign.sign === Sign.Plus ? "+" : "-";
  return `${termExprKey(st.termExpr)}|${posKey}|${signKey}`;
}

function termsMatchForConclusion(a: SignedTerm, b: SignedTerm): boolean {
  return (
    signedTermEq(a, b) ||
    (termExprKey(a.termExpr) === termExprKey(b.termExpr) &&
      a.positions.length === b.positions.length &&
      a.positions.every((p, i) => p === b.positions[i]) &&
      signsCompatible(a.sign, b.sign))
  );
}

function signsCompatible(a: WildSign, b: WildSign): boolean {
  if (a.tag === "Wild" || b.tag === "Wild") return true;
  return a.sign === b.sign;
}

function diffTerms(
  us: readonly SignedTerm[],
  cs: readonly SignedTerm[],
): [SignedTerm[], SignedTerm[]] {
  let i = 0;
  let j = 0;
  const missing: SignedTerm[] = [];
  const extra: SignedTerm[] = [];

  while (i < us.length && j < cs.length) {
    if (termsMatchForConclusion(us[i], cs[j])) {
      i++;
      j++;
    } else {
      const cmp = termKey(us[i]).localeCompare(termKey(cs[j]));
      if (cmp < 0) {
        missing.push(us[i]);
        i++;
      } else {
        extra.push(cs[j]);
        j++;
      }
    }
  }
  while (i < us.length) {
    missing.push(us[i]);
    i++;
  }
  while (j < cs.length) {
    extra.push(cs[j]);
    j++;
  }
  return [missing, extra];
}

function prettySignedTermBrief(st: SignedTerm): string {
  const signStr =
    st.sign.tag === "Wild" ? "*" : st.sign.sign === Sign.Plus ? "+" : "\u2212";
  const posStr =
    st.positions.length === 0 ? "" : "<" + st.positions.join(",") + ">";
  if (st.termExpr.tag === "Atomic") {
    const comp = st.termExpr.term.complemented ? "non-" : "";
    return signStr + comp + st.termExpr.term.termName + posStr;
  }
  return signStr + "(...)" + posStr;
}
