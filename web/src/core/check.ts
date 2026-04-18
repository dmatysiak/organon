// Document checker — ported from Organon.Syl.Check

import type { PropositionH, Solution, SolutionProp, SyllogismH } from "./hole";
import { solve } from "./hole";
import type {
  Located,
  Premise,
  ProofBlock,
  Document,
  SrcPos,
} from "./document";
import { fromConcreteH } from "./document";
import {
  prettyFigure,
  prettyMood,
  prettyProof,
  prettyProposition,
  prettyPropositionH,
} from "./pretty";
import { reduce } from "./proof";
import {
  figure,
  Mood,
  ProofStep,
  Proposition,
  PropType,
  Syllogism,
  Tradition,
} from "./types";
import { validate } from "./validity";

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
  readonly checkedMood: Mood;
  readonly checkedSyllogism: Syllogism;
  readonly checkedSwapped: boolean;
  readonly checkedSteps: ProofStep[];
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

export type SwapAction = {
  readonly swapPrem1Start: SrcPos;
  readonly swapPrem1End: SrcPos;
  readonly swapPrem2Start: SrcPos;
  readonly swapPrem2End: SrcPos;
};

export type HoleFillEdit = {
  readonly fillEditStart: SrcPos;
  readonly fillEditEnd: SrcPos;
  readonly fillEditText: string;
};

export type HoleFill = {
  readonly holeFillMood: Mood;
  readonly holeFillEdits: HoleFillEdit[];
  readonly holeFillLabel: string;
};

export type NamespaceEntry = {
  readonly nsFilePath: string;
  readonly nsConclusions: Map<string, Proposition>;
  readonly nsLocations: Map<string, [SrcPos, SrcPos]>;
};

export type ExternalContext = Map<string, NamespaceEntry>;

export type CheckResult = {
  readonly checkDiagnostics: Diagnostic[];
  readonly checkProofs: CheckedProof[];
  readonly checkHovers: HoverItem[];
  readonly checkDefinitions: DefinitionItem[];
  readonly checkSwaps: SwapAction[];
  readonly checkHoleFills: HoleFill[];
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function propToH(p: Proposition): PropositionH {
  return {
    tag: "PropH",
    propTypeH: { tag: "ConcretePT", propType: p.propType },
    subject: { tag: "ConcreteT", term: p.subject },
    predicate: { tag: "ConcreteT", term: p.predicate },
  };
}

function solutionPropToConc(sp: SolutionProp): Proposition | null {
  if (sp.solSubject !== null && sp.solPredicate !== null) {
    return {
      propType: sp.solPropType,
      subject: sp.solSubject,
      predicate: sp.solPredicate,
    };
  }
  return null;
}

function hasHoles(ph: PropositionH): boolean {
  if (ph.tag === "WholePropH") return true;
  if (
    ph.propTypeH.tag === "ConcretePT" &&
    ph.subject.tag === "ConcreteT" &&
    ph.predicate.tag === "ConcreteT"
  )
    return false;
  return true;
}

// ---------------------------------------------------------------------------
// Resolve premises
// ---------------------------------------------------------------------------

function resolveSingle(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Proposition>,
  lp: Located<Premise>,
): { ok: PropositionH } | { err: Diagnostic[] } {
  const prem = lp.locValue;

  if (prem.tag === "PremiseProp") return { ok: propToH(prem.prop) };
  if (prem.tag === "PremiseHole") return { ok: prem.propH };

  // PremiseRef
  if (prem.namespace === null) {
    // Unqualified: try local first
    const local = ctx.get(prem.name);
    if (local) return { ok: propToH(local) };

    // Then opened namespaces
    const hits: Proposition[] = [];
    for (const ns of opens) {
      const entry = ext.get(ns);
      if (entry) {
        const p = entry.nsConclusions.get(prem.name);
        if (p) hits.push(p);
      }
    }
    if (hits.length === 1) return { ok: propToH(hits[0]) };
    if (hits.length > 1) {
      return {
        err: [
          {
            diagStart: lp.locStart,
            diagEnd: lp.locEnd,
            diagSeverity: Severity.Error,
            diagMessage: `Ambiguous reference: @${prem.name} (found in multiple opened namespaces)`,
          },
        ],
      };
    }
    return {
      err: [
        {
          diagStart: lp.locStart,
          diagEnd: lp.locEnd,
          diagSeverity: Severity.Error,
          diagMessage: `Unknown reference: @${prem.name}`,
        },
      ],
    };
  } else {
    // Qualified
    const entry = ext.get(prem.namespace);
    if (!entry) {
      return {
        err: [
          {
            diagStart: lp.locStart,
            diagEnd: lp.locEnd,
            diagSeverity: Severity.Error,
            diagMessage: `Unknown namespace: ${prem.namespace}`,
          },
        ],
      };
    }
    const p = entry.nsConclusions.get(prem.name);
    if (p) return { ok: propToH(p) };
    return {
      err: [
        {
          diagStart: lp.locStart,
          diagEnd: lp.locEnd,
          diagSeverity: Severity.Error,
          diagMessage: `Unknown reference: @${prem.namespace}.${prem.name}`,
        },
      ],
    };
  }
}

function resolvePremises(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Proposition>,
  prems: Located<Premise>[],
): { ok: PropositionH[] } | { err: Diagnostic[] } {
  const results: PropositionH[] = [];
  for (const lp of prems) {
    const r = resolveSingle(ext, opens, ctx, lp);
    if ("err" in r) return r;
    results.push(r.ok);
  }
  return { ok: results };
}

// ---------------------------------------------------------------------------
// Resolve reference (for hovers)
// ---------------------------------------------------------------------------

function resolveRef(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Proposition>,
  ns: string | null,
  name: string,
): Proposition | null {
  if (ns === null) {
    const local = ctx.get(name);
    if (local) return local;
    for (const nsName of opens) {
      const entry = ext.get(nsName);
      if (entry) {
        const p = entry.nsConclusions.get(name);
        if (p) return p;
      }
    }
    return null;
  }
  const entry = ext.get(ns);
  if (!entry) return null;
  return entry.nsConclusions.get(name) ?? null;
}

// ---------------------------------------------------------------------------
// Hover, definition, swap, hole-fill builders
// ---------------------------------------------------------------------------

function mkProofHover(s: SrcPos, e: SrcPos, cp: CheckedProof): HoverItem {
  const fig = figure(cp.checkedSyllogism);
  const figText = fig !== null ? `Figure ${prettyFigure(fig)}, ` : "";
  const swapNote = cp.checkedSwapped ? " (premises swapped)" : "";
  const header = `${figText}${prettyMood(cp.checkedMood)}${swapNote}`;
  const body = prettyProof(cp.checkedMood, cp.checkedSteps);
  return { hoverStart: s, hoverEnd: e, hoverText: `${header}\n\n${body}` };
}

function mkRefHovers(
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Proposition>,
  prems: Located<Premise>[],
): HoverItem[] {
  const items: HoverItem[] = [];
  for (const lp of prems) {
    const prem = lp.locValue;
    if (prem.tag === "PremiseRef") {
      const prop = resolveRef(ext, opens, ctx, prem.namespace, prem.name);
      if (prop) {
        items.push({
          hoverStart: lp.locStart,
          hoverEnd: lp.locEnd,
          hoverText: prettyProposition(prop),
        });
      }
    }
  }
  return items;
}

function mkRefDefs(
  ext: ExternalContext,
  opens: string[],
  locs: Map<string, [SrcPos, SrcPos]>,
  prems: Located<Premise>[],
): DefinitionItem[] {
  const items: DefinitionItem[] = [];
  for (const lp of prems) {
    const prem = lp.locValue;
    if (prem.tag !== "PremiseRef") continue;

    if (prem.namespace === null) {
      // Try local
      const local = locs.get(prem.name);
      if (local) {
        items.push({
          defRefStart: lp.locStart,
          defRefEnd: lp.locEnd,
          defTargetFile: null,
          defTargetStart: local[0],
          defTargetEnd: local[1],
        });
        continue;
      }
      // Try opened namespaces
      for (const ns of opens) {
        const entry = ext.get(ns);
        if (entry) {
          const loc = entry.nsLocations.get(prem.name);
          if (loc) {
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
      const entry = ext.get(prem.namespace);
      if (entry) {
        const loc = entry.nsLocations.get(prem.name);
        if (loc) {
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

function mkSwapAction(
  cp: CheckedProof,
  prems: Located<Premise>[],
): SwapAction[] {
  if (cp.checkedSwapped && prems.length === 2) {
    return [
      {
        swapPrem1Start: prems[0].locStart,
        swapPrem1End: prems[0].locEnd,
        swapPrem2Start: prems[1].locStart,
        swapPrem2End: prems[1].locEnd,
      },
    ];
  }
  return [];
}

function mkHoleEdits(
  s: SrcPos,
  e: SrcPos,
  propH: PropositionH,
  solProp: SolutionProp,
): HoleFillEdit[] {
  if (!hasHoles(propH)) return [];
  const concl = solutionPropToConc(solProp);
  if (concl === null) return [];
  return [
    {
      fillEditStart: s,
      fillEditEnd: e,
      fillEditText: prettyProposition(concl),
    },
  ];
}

function mkSolutionFill(
  premLocs: Located<Premise>[],
  p1h: PropositionH,
  p2h: PropositionH,
  conclH: PropositionH,
  conclLoc: Located<PropositionH>,
  sol: Solution,
): HoleFill {
  if (premLocs.length !== 2) {
    return { holeFillMood: Mood.Barbara, holeFillEdits: [], holeFillLabel: "" };
  }
  const [loc1, loc2] = premLocs;
  const edits = [
    ...mkHoleEdits(loc1.locStart, loc1.locEnd, p1h, sol.solMajor),
    ...mkHoleEdits(loc2.locStart, loc2.locEnd, p2h, sol.solMinor),
    ...mkHoleEdits(
      conclLoc.locStart,
      conclLoc.locEnd,
      conclH,
      sol.solConclusion,
    ),
  ];
  const conc = solutionPropToConc(sol.solConclusion);
  const label =
    conc !== null ? prettyProposition(conc) : prettyPropositionH(conclH);
  return {
    holeFillMood: sol.solutionMood,
    holeFillEdits: edits,
    holeFillLabel: label,
  };
}

function collectHoleSpans(
  premLocs: Located<Premise>[],
  p1h: PropositionH,
  p2h: PropositionH,
  conclH: PropositionH,
  conclLoc: Located<PropositionH>,
): [SrcPos, SrcPos][] {
  if (premLocs.length !== 2) return [];
  const spans: [SrcPos, SrcPos][] = [];
  if (hasHoles(p1h)) spans.push([premLocs[0].locStart, premLocs[0].locEnd]);
  if (hasHoles(p2h)) spans.push([premLocs[1].locStart, premLocs[1].locEnd]);
  if (hasHoles(conclH)) spans.push([conclLoc.locStart, conclLoc.locEnd]);
  return spans;
}

// ---------------------------------------------------------------------------
// Check a single proof block
// ---------------------------------------------------------------------------

function checkProofBlock(
  trad: Tradition,
  ext: ExternalContext,
  opens: string[],
  ctx: Map<string, Proposition>,
  block: ProofBlock,
): { ok: CheckedProof } | { err: [Diagnostic[], HoleFill[]] } {
  const resolved = resolvePremises(ext, opens, ctx, block.proofPremises);
  if ("err" in resolved) return { err: [resolved.err, []] };

  const props = resolved.ok;
  if (props.length !== 2) {
    const s = block.proofName.locStart;
    const e = block.proofName.locEnd;
    return {
      err: [
        [
          {
            diagStart: s,
            diagEnd: e,
            diagSeverity: Severity.Error,
            diagMessage: `Expected 2 premises, got ${props.length}`,
          },
        ],
        [],
      ],
    };
  }

  const [p1h, p2h] = props;
  const conclH = block.proofConclusion.locValue;
  const p1 = fromConcreteH(p1h);
  const p2 = fromConcreteH(p2h);
  const cc = fromConcreteH(conclH);

  if (p1 !== null && p2 !== null && cc !== null) {
    // All concrete: validate
    const syl: Syllogism = { major: p1, minor: p2, conclusion: cc };
    const result = validate(trad, syl);
    switch (result.tag) {
      case "Valid": {
        const steps = reduce(result.mood, syl);
        return {
          ok: {
            checkedName: block.proofName.locValue,
            checkedMood: result.mood,
            checkedSyllogism: syl,
            checkedSwapped: false,
            checkedSteps: steps,
          },
        };
      }
      case "ValidSwapped": {
        const steps = reduce(result.mood, result.syllogism);
        return {
          ok: {
            checkedName: block.proofName.locValue,
            checkedMood: result.mood,
            checkedSyllogism: result.syllogism,
            checkedSwapped: true,
            checkedSteps: steps,
          },
        };
      }
      case "Invalid": {
        const s = block.proofConclusion.locStart;
        const e = block.proofConclusion.locEnd;
        return {
          err: [
            [
              {
                diagStart: s,
                diagEnd: e,
                diagSeverity: Severity.Error,
                diagMessage: result.message,
              },
            ],
            [],
          ],
        };
      }
    }
  }

  // At least one hole: run solver
  const sylH: SyllogismH = { majorH: p1h, minorH: p2h, conclusionH: conclH };
  const solutions = solve(trad, sylH);
  const fills = solutions.map((sol) =>
    mkSolutionFill(
      block.proofPremises,
      p1h,
      p2h,
      conclH,
      block.proofConclusion,
      sol,
    ),
  );
  const holeSpans = collectHoleSpans(
    block.proofPremises,
    p1h,
    p2h,
    conclH,
    block.proofConclusion,
  );
  const diags: Diagnostic[] = holeSpans.map(([s, e]) => ({
    diagStart: s,
    diagEnd: e,
    diagSeverity: Severity.Warning,
    diagMessage: `${fills.length} solution(s) available`,
  }));

  return { err: [diags, fills] };
}

// ---------------------------------------------------------------------------
// Main entry point
// ---------------------------------------------------------------------------

export function checkDocument(
  ext: ExternalContext,
  doc: Document,
): CheckResult {
  const tradition =
    doc.docTradition !== null ? doc.docTradition.locValue : Tradition.Full;
  const opens = doc.docOpens.map((lo) => lo.locValue);

  // Diagnose unknown open directives
  const openDiags: Diagnostic[] = [];
  for (const lo of doc.docOpens) {
    if (!ext.has(lo.locValue)) {
      openDiags.push({
        diagStart: lo.locStart,
        diagEnd: lo.locEnd,
        diagSeverity: Severity.Error,
        diagMessage: `Unknown namespace: ${lo.locValue}`,
      });
    }
  }

  const diags: Diagnostic[] = [];
  const proofs: CheckedProof[] = [];
  const hovers: HoverItem[] = [];
  const defs: DefinitionItem[] = [];
  const swaps: SwapAction[] = [];
  const fills: HoleFill[] = [];

  const ctx = new Map<string, Proposition>();
  const locs = new Map<string, [SrcPos, SrcPos]>();

  for (const lp of doc.docProofs) {
    const block = lp.locValue;
    const name = block.proofName.locValue;
    const nameStart = block.proofName.locStart;
    const nameEnd = block.proofName.locEnd;

    // Reference hovers and definitions are independent of proof validity
    hovers.push(...mkRefHovers(ext, opens, ctx, block.proofPremises));
    defs.push(...mkRefDefs(ext, opens, locs, block.proofPremises));

    const result = checkProofBlock(tradition, ext, opens, ctx, block);

    if ("err" in result) {
      const [newDiags, newFills] = result.err;
      diags.push(...newDiags);
      fills.push(...newFills);
    } else {
      const checked = result.ok;
      if (ctx.has(name)) {
        diags.push({
          diagStart: nameStart,
          diagEnd: nameEnd,
          diagSeverity: Severity.Error,
          diagMessage: `Duplicate proof name: ${name}`,
        });
      } else {
        ctx.set(name, checked.checkedSyllogism.conclusion);
        locs.set(name, [nameStart, nameEnd]);
        proofs.push(checked);
        hovers.push(mkProofHover(nameStart, nameEnd, checked));
        swaps.push(...mkSwapAction(checked, block.proofPremises));
      }
    }
  }

  return {
    checkDiagnostics: [...openDiags, ...diags],
    checkProofs: proofs,
    checkHovers: hovers,
    checkDefinitions: defs,
    checkSwaps: swaps,
    checkHoleFills: fills,
  };
}
