// TFL Document checker — ported from Organon.Tfl.Check

import type { StatementH } from "./hole";
import type {
  Located,
  Premise,
  ProofBlock,
  Document,
  RefModifier,
  SrcPos,
} from "./document";
import { fromConcreteH } from "./document";
import {
  prettyCancellation,
  prettySignedTerm,
  prettyStatement,
  prettyStatementEnglish,
} from "./pretty";
import {
  Sign,
  type SignedTerm,
  type Statement,
  type Inference,
  type TermExpr,
  type WildSign,
  type RelLexicon,
  Atomic,
  Compound,
  flipSign,
  Fixed,
  Wild,
} from "./types";
import {
  cancellation,
  validate,
  type Cancellation,
  type ValidationResult,
} from "./validity";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export enum Severity {
  Error = "Error",
  Warning = "Warning",
}

export type Diagnostic = {
  readonly diagStart: SrcPos;
  readonly diagEnd: SrcPos;
  readonly diagSeverity: Severity;
  readonly diagMessage: string;
};

export type CheckedProof = {
  readonly checkedName: string;
  readonly checkedInference: Inference;
  readonly checkedCancellation: Cancellation;
};

export type HoverItem = {
  readonly hoverStart: SrcPos;
  readonly hoverEnd: SrcPos;
  readonly hoverText: string;
};

export type DefinitionItem = {
  readonly defRefStart: SrcPos;
  readonly defRefEnd: SrcPos;
  readonly defTargetFile: string | null;
  readonly defTargetStart: SrcPos;
  readonly defTargetEnd: SrcPos;
};

export type HoleFillEdit = {
  readonly fillEditStart: SrcPos;
  readonly fillEditEnd: SrcPos;
  readonly fillEditText: string;
};

export type HoleFill = {
  readonly holeFillEdits: HoleFillEdit[];
  readonly holeFillLabel: string;
};

export type NamespaceEntry = {
  readonly nsFilePath: string;
  readonly nsConclusions: Map<string, Statement>;
  readonly nsLocations: Map<string, [SrcPos, SrcPos]>;
};

export type ExternalContext = {
  readonly namespaces: Map<string, NamespaceEntry>;
};

export type CheckResult = {
  readonly checkDiagnostics: Diagnostic[];
  readonly checkProofs: CheckedProof[];
  readonly checkHovers: HoverItem[];
  readonly checkDefinitions: DefinitionItem[];
  readonly checkHoleFills: HoleFill[];
};

// ---------------------------------------------------------------------------
// Main entry point
// ---------------------------------------------------------------------------

export function checkDocument(
  ext: ExternalContext,
  doc: Document,
): CheckResult {
  const opens = doc.docOpens.map((lo) => lo.locValue);
  const rl = doc.docRelLexicon;
  const openDiags: Diagnostic[] = doc.docOpens
    .filter((lo) => !ext.namespaces.has(lo.locValue))
    .map((lo) => ({
      diagStart: lo.locStart,
      diagEnd: lo.locEnd,
      diagSeverity: Severity.Error,
      diagMessage: `Unknown namespace: ${lo.locValue}`,
    }));

  const diags: Diagnostic[] = [...openDiags];
  const proofs: CheckedProof[] = [];
  const hovers: HoverItem[] = [];
  const defs: DefinitionItem[] = [];
  const fills: HoleFill[] = [];

  const ctx = new Map<string, Statement>();
  const locs = new Map<string, [SrcPos, SrcPos]>();

  for (const lp of doc.docProofs) {
    const block = lp.locValue;
    const name = block.proofName.locValue;
    const nameStart = block.proofName.locStart;
    const nameEnd = block.proofName.locEnd;

    // Collect reference hovers and definitions
    hovers.push(...mkRefHovers(rl, ext, opens, ctx, block.proofPremises));
    defs.push(...mkRefDefs(ext, opens, locs, block.proofPremises));

    const result = checkProofBlock(ext, opens, ctx, block);
    if (result.tag === "error") {
      diags.push(...result.diags);
      fills.push(...result.fills);
    } else {
      const checked = result.checked;
      if (ctx.has(name)) {
        diags.push({
          diagStart: nameStart,
          diagEnd: nameEnd,
          diagSeverity: Severity.Error,
          diagMessage: `Duplicate proof name: ${name}`,
        });
      } else {
        ctx.set(name, checked.checkedInference.conclusion);
        locs.set(name, [nameStart, nameEnd]);
        proofs.push(checked);
        hovers.push(mkProofHover(rl, nameStart, nameEnd, checked));
      }
    }
  }

  return {
    checkDiagnostics: diags,
    checkProofs: proofs,
    checkHovers: hovers,
    checkDefinitions: defs,
    checkHoleFills: fills,
  };
}

// ---------------------------------------------------------------------------
// Proof block checking
// ---------------------------------------------------------------------------

type CheckBlockResult =
  | { tag: "ok"; checked: CheckedProof }
  | { tag: "error"; diags: Diagnostic[]; fills: HoleFill[] };

function checkProofBlock(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Statement>,
  block: ProofBlock,
): CheckBlockResult {
  const resolved = resolvePremises(ext, opens, ctx, block.proofPremises);
  if (resolved.tag === "error") {
    return { tag: "error", diags: resolved.diags, fills: [] };
  }

  const stmtHs = resolved.stmtHs;
  const conclH = block.proofConclusion.locValue;
  const concreteStmts = allConcrete(stmtHs);
  const concreteConclusion = fromConcreteH(conclH);

  if (concreteStmts !== null && concreteConclusion !== null) {
    const inf: Inference = {
      premises: concreteStmts,
      conclusion: concreteConclusion,
    };
    const result = validate(inf);
    if (result.tag === "Valid") {
      return {
        tag: "ok",
        checked: {
          checkedName: block.proofName.locValue,
          checkedInference: inf,
          checkedCancellation: result.cancellation,
        },
      };
    }
    const diags = result.errors.map((msg) => ({
      diagStart: block.proofConclusion.locStart,
      diagEnd: block.proofConclusion.locEnd,
      diagSeverity: Severity.Error as const,
      diagMessage: msg,
    }));
    return { tag: "error", diags, fills: [] };
  }

  if (concreteStmts !== null && concreteConclusion === null) {
    // Solve conclusion from premises
    const cancel = cancellation(concreteStmts);
    const uncancelledSTs = cancel.uncancelled.map(([_, st]) => st);
    const solvedConcl: Statement = { terms: uncancelledSTs };
    const fill: HoleFill = {
      holeFillEdits: [
        {
          fillEditStart: block.proofConclusion.locStart,
          fillEditEnd: block.proofConclusion.locEnd,
          fillEditText: prettyStatement(solvedConcl),
        },
      ],
      holeFillLabel: prettyStatement(solvedConcl),
    };
    const diag: Diagnostic = {
      diagStart: block.proofConclusion.locStart,
      diagEnd: block.proofConclusion.locEnd,
      diagSeverity: Severity.Warning,
      diagMessage: `Solved conclusion: ${prettyStatement(solvedConcl)}`,
    };
    return { tag: "error", diags: [diag], fills: [fill] };
  }

  // Premises have holes
  return {
    tag: "error",
    diags: [
      {
        diagStart: block.proofName.locStart,
        diagEnd: block.proofName.locEnd,
        diagSeverity: Severity.Warning,
        diagMessage: "Cannot validate: premises contain holes",
      },
    ],
    fills: [],
  };
}

function allConcrete(stmtHs: StatementH[]): Statement[] | null {
  const result: Statement[] = [];
  for (const sh of stmtHs) {
    const s = fromConcreteH(sh);
    if (s === null) return null;
    result.push(s);
  }
  return result;
}

// ---------------------------------------------------------------------------
// Reference resolution
// ---------------------------------------------------------------------------

type ResolveResult =
  | { tag: "ok"; stmtHs: StatementH[] }
  | { tag: "error"; diags: Diagnostic[] };

function resolvePremises(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Statement>,
  premises: Located<Premise>[],
): ResolveResult {
  const stmtHs: StatementH[] = [];
  for (const lp of premises) {
    const result = resolveSingle(ext, opens, ctx, lp);
    if (result.tag === "error") return result;
    stmtHs.push(result.stmtH);
  }
  return { tag: "ok", stmtHs };
}

type ResolveSingleResult =
  | { tag: "ok"; stmtH: StatementH }
  | { tag: "error"; diags: Diagnostic[] };

function resolveSingle(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Statement>,
  lp: Located<Premise>,
): ResolveSingleResult {
  const prem = lp.locValue;

  if (prem.tag === "PremiseStmt") {
    return { tag: "ok", stmtH: stmtToH(prem.stmt) };
  }

  if (prem.tag === "PremiseHole") {
    return { tag: "ok", stmtH: prem.stmtH };
  }

  // PremiseRef
  const stmt = resolveRef(ext, opens, ctx, prem.namespace, prem.name);
  if (stmt === null) {
    const label =
      prem.namespace !== null
        ? `@${prem.namespace}.${prem.name}`
        : `@${prem.name}`;
    return {
      tag: "error",
      diags: [
        {
          diagStart: lp.locStart,
          diagEnd: lp.locEnd,
          diagSeverity: Severity.Error,
          diagMessage: `Unknown reference: ${label}`,
        },
      ],
    };
  }

  return applyRefModifier(lp, prem.modifier, stmt);
}

function resolveRef(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Statement>,
  namespace: string | null,
  name: string,
): Statement | null {
  if (namespace !== null) {
    const entry = ext.namespaces.get(namespace);
    if (entry === undefined) return null;
    return entry.nsConclusions.get(name) ?? null;
  }

  // Check local context first
  const local = ctx.get(name);
  if (local !== undefined) return local;

  // Check open namespaces
  const hits: Statement[] = [];
  for (const ns of opens) {
    const entry = ext.namespaces.get(ns);
    if (entry === undefined) continue;
    const stmt = entry.nsConclusions.get(name);
    if (stmt !== undefined) hits.push(stmt);
  }
  return hits.length === 1 ? hits[0] : null;
}

// ---------------------------------------------------------------------------
// Reference modifiers
// ---------------------------------------------------------------------------

function applyRefModifier(
  lp: Located<Premise>,
  modifier: RefModifier | null,
  stmt: Statement,
): ResolveSingleResult {
  if (modifier === null) return { tag: "ok", stmtH: stmtToH(stmt) };

  let result: Statement | null = null;
  let errMsg = "";

  switch (modifier) {
    case "conv":
      result = applyConv(stmt);
      errMsg =
        "Cannot apply simple conversion: requires a 2-term E or I statement (symmetric signs)";
      break;
    case "per-accidens":
      result = applyPerAccidens(stmt);
      errMsg =
        "Cannot apply conversion per accidens: requires a 2-term A or E statement";
      break;
    case "obv":
      return { tag: "ok", stmtH: stmtToH(applyObv(stmt)) };
    case "contra":
      result = applyContra(stmt);
      errMsg =
        "Cannot apply contraposition: requires a 2-term A or O statement";
      break;
  }

  if (result !== null) return { tag: "ok", stmtH: stmtToH(result) };
  return {
    tag: "error",
    diags: [
      {
        diagStart: lp.locStart,
        diagEnd: lp.locEnd,
        diagSeverity: Severity.Error,
        diagMessage: errMsg,
      },
    ],
  };
}

export function applyConv(stmt: Statement): Statement | null {
  if (stmt.terms.length !== 2) return null;
  const [st1, st2] = stmt.terms;
  if (
    st1.sign.tag === "Fixed" &&
    st2.sign.tag === "Fixed" &&
    st1.sign.sign === st2.sign.sign
  ) {
    return { terms: [st2, st1] };
  }
  return null;
}

export function applyPerAccidens(stmt: Statement): Statement | null {
  if (stmt.terms.length !== 2) return null;
  const [st1, st2] = stmt.terms;
  if (
    st1.sign.tag === "Fixed" &&
    st1.sign.sign === Sign.Minus &&
    st2.sign.tag === "Fixed" &&
    st2.sign.sign === Sign.Plus
  ) {
    // A: -S +P → +P +S
    return {
      terms: [
        {
          sign: Fixed(Sign.Plus),
          termExpr: st2.termExpr,
          positions: [...st2.positions],
        },
        {
          sign: Fixed(Sign.Plus),
          termExpr: st1.termExpr,
          positions: [...st1.positions],
        },
      ],
    };
  }
  if (
    st1.sign.tag === "Fixed" &&
    st1.sign.sign === Sign.Minus &&
    st2.sign.tag === "Fixed" &&
    st2.sign.sign === Sign.Minus
  ) {
    // E: -S -P → +P -S
    return {
      terms: [
        {
          sign: Fixed(Sign.Plus),
          termExpr: st2.termExpr,
          positions: [...st2.positions],
        },
        {
          sign: Fixed(Sign.Minus),
          termExpr: st1.termExpr,
          positions: [...st1.positions],
        },
      ],
    };
  }
  return null;
}

export function applyObv(stmt: Statement): Statement {
  if (stmt.terms.length === 0) return stmt;
  const prefix = stmt.terms.slice(0, -1);
  const lst = stmt.terms[stmt.terms.length - 1];
  const flipped: SignedTerm = {
    sign: flipWildSign(lst.sign),
    termExpr: complementTermExpr(lst.termExpr),
    positions: [...lst.positions],
  };
  return { terms: [...prefix, flipped] };
}

export function applyContra(stmt: Statement): Statement | null {
  if (stmt.terms.length !== 2) return null;
  const [st1, st2] = stmt.terms;
  if (isAPattern(st1, st2) || isOPattern(st1, st2)) {
    return {
      terms: [
        {
          sign: st2.sign,
          termExpr: complementTermExpr(st2.termExpr),
          positions: [...st2.positions],
        },
        {
          sign: st1.sign,
          termExpr: complementTermExpr(st1.termExpr),
          positions: [...st1.positions],
        },
      ],
    };
  }
  return null;
}

function isAPattern(st1: SignedTerm, st2: SignedTerm): boolean {
  return (
    st1.sign.tag === "Fixed" &&
    st1.sign.sign === Sign.Minus &&
    st2.sign.tag === "Fixed" &&
    st2.sign.sign === Sign.Plus
  );
}

function isOPattern(st1: SignedTerm, st2: SignedTerm): boolean {
  return (
    st1.sign.tag === "Fixed" &&
    st1.sign.sign === Sign.Plus &&
    st2.sign.tag === "Fixed" &&
    st2.sign.sign === Sign.Minus
  );
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function stmtToH(stmt: Statement): StatementH {
  return {
    tag: "StmtH",
    terms: stmt.terms.map((st) => ({ tag: "ConcreteSTH" as const, st })),
  };
}

function flipWildSign(ws: WildSign): WildSign {
  if (ws.tag === "Fixed") return Fixed(flipSign(ws.sign));
  return Wild;
}

function complementTermExpr(te: TermExpr): TermExpr {
  if (te.tag === "Atomic") {
    return Atomic({
      termName: te.term.termName,
      complemented: !te.term.complemented,
    });
  }
  return Compound(
    te.elements.map((st) => ({
      ...st,
      termExpr: complementTermExpr(st.termExpr),
    })),
  );
}

// ---------------------------------------------------------------------------
// Hover items
// ---------------------------------------------------------------------------

function mkProofHover(
  rl: RelLexicon,
  s: SrcPos,
  e: SrcPos,
  cp: CheckedProof,
): HoverItem {
  const inf = cp.checkedInference;
  const cancel = cp.checkedCancellation;
  const nPrems = inf.premises.length;
  const header = `TFL inference (${nPrems} premise${nPrems === 1 ? "" : "s"})`;
  const algebraic = prettyStatement(inf.conclusion);
  const english = prettyStatementEnglish(rl, inf.conclusion);
  const conclLine = `Conclusion: ${algebraic}  (${english})`;
  const cancelInfo = prettyCancellation(cancel);
  return {
    hoverStart: s,
    hoverEnd: e,
    hoverText: `${header}\n${conclLine}\n\n${cancelInfo}`,
  };
}

function mkRefHovers(
  rl: RelLexicon,
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Statement>,
  premises: Located<Premise>[],
): HoverItem[] {
  const items: HoverItem[] = [];
  for (const lp of premises) {
    const prem = lp.locValue;
    if (prem.tag !== "PremiseRef") continue;
    const stmt = resolveRef(ext, opens, ctx, prem.namespace, prem.name);
    if (stmt === null) continue;
    const base = `${prettyStatement(stmt)}  (${prettyStatementEnglish(rl, stmt)})`;
    let text = base;
    if (prem.modifier !== null) {
      const applied = applyRefModifier(lp, prem.modifier, stmt);
      if (applied.tag === "ok") {
        const s = fromConcreteH(applied.stmtH);
        if (s !== null) {
          text = `${base}\n\u2192 ${prettyStatement(s)}  (${prettyStatementEnglish(rl, s)})`;
        }
      }
    }
    items.push({
      hoverStart: lp.locStart,
      hoverEnd: lp.locEnd,
      hoverText: text,
    });
  }
  return items;
}

// ---------------------------------------------------------------------------
// Definition items
// ---------------------------------------------------------------------------

function mkRefDefs(
  ext: ExternalContext,
  opens: string[],
  locs: Map<string, [SrcPos, SrcPos]>,
  premises: Located<Premise>[],
): DefinitionItem[] {
  const items: DefinitionItem[] = [];
  for (const lp of premises) {
    const prem = lp.locValue;
    if (prem.tag !== "PremiseRef") continue;

    if (prem.namespace === null) {
      const local = locs.get(prem.name);
      if (local !== undefined) {
        items.push({
          defRefStart: lp.locStart,
          defRefEnd: lp.locEnd,
          defTargetFile: null,
          defTargetStart: local[0],
          defTargetEnd: local[1],
        });
      } else {
        for (const ns of opens) {
          const entry = ext.namespaces.get(ns);
          if (entry === undefined) continue;
          const loc = entry.nsLocations.get(prem.name);
          if (loc !== undefined) {
            items.push({
              defRefStart: lp.locStart,
              defRefEnd: lp.locEnd,
              defTargetFile: entry.nsFilePath,
              defTargetStart: loc[0],
              defTargetEnd: loc[1],
            });
            break;
          }
        }
      }
    } else {
      const entry = ext.namespaces.get(prem.namespace);
      if (entry !== undefined) {
        const loc = entry.nsLocations.get(prem.name);
        if (loc !== undefined) {
          items.push({
            defRefStart: lp.locStart,
            defRefEnd: lp.locEnd,
            defTargetFile: entry.nsFilePath,
            defTargetStart: loc[0],
            defTargetEnd: loc[1],
          });
        }
      }
    }
  }
  return items;
}
