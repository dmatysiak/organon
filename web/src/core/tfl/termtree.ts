// Englebretsen term tree — visualizes U/N/E/I relations between terms
// Reference: Englebretsen, "Robust Reality" (2012)

import { Sign, type Term, type SignedTerm, type Statement } from "./types";
import {
  NODE_H,
  nodeWidth,
  createSvg,
  appendBackground,
  drawNode,
  drawNodeEdge,
  drawLegend,
} from "./treeutil";

// ---------------------------------------------------------------------------
// Relation types
// ---------------------------------------------------------------------------

/** U-relation: from -A +B (every A is B), A is contained in B. */
export type UEdge = {
  readonly tag: "U";
  readonly from: string; // term key (name + complemented)
  readonly to: string;
  readonly proof: string; // proof name that established this
};

/** E-relation: from -A -B (no A is B), A and B are disjoint. */
export type EEdge = {
  readonly tag: "E";
  readonly term1: string;
  readonly term2: string;
  readonly proof: string;
};

/** I-relation: from +A +B (some A is B), A and B overlap. */
export type IEdge = {
  readonly tag: "I";
  readonly term1: string;
  readonly term2: string;
  readonly proof: string;
};

/** O-relation: from +A -B (some A is not B), not all A are B. */
export type OEdge = {
  readonly tag: "O";
  readonly from: string;
  readonly to: string;
  readonly proof: string;
};

export type TermRelation = UEdge | EEdge | IEdge | OEdge;

// ---------------------------------------------------------------------------
// Term tree data
// ---------------------------------------------------------------------------

export type TermTreeData = {
  /** All unique term keys. */
  readonly terms: readonly string[];
  /** Direct U-edges (containment). */
  readonly uEdges: readonly UEdge[];
  /** E-edges (disjointness). */
  readonly eEdges: readonly EEdge[];
  /** I-edges (overlap). */
  readonly iEdges: readonly IEdge[];
  /** O-edges. */
  readonly oEdges: readonly OEdge[];
  /** Transitive U-closure edges (derived). */
  readonly uClosure: readonly UEdge[];
  /** N-pairs: [term, non-term] for complemented terms that appear. */
  readonly nPairs: readonly [string, string][];
};

// ---------------------------------------------------------------------------
// Extract relations from statements
// ---------------------------------------------------------------------------

function termKey(t: Term): string {
  return t.complemented ? "non-" + t.termName : t.termName;
}

/** Extract the term relation from a 2-term monadic statement, if any. */
function extractRelation(
  stmt: Statement,
  proofName: string,
): TermRelation | null {
  const ts = stmt.terms;
  if (ts.length !== 2) return null;
  const [a, b] = ts;
  // Only monadic (no positional subscripts)
  if (a.positions.length > 0 || b.positions.length > 0) return null;
  // Only atomic terms
  if (a.termExpr.tag !== "Atomic" || b.termExpr.tag !== "Atomic") return null;

  const aKey = termKey(a.termExpr.term);
  const bKey = termKey(b.termExpr.term);
  const aSign = a.sign;
  const bSign = b.sign;

  if (aSign.tag !== "Fixed" || bSign.tag !== "Fixed") return null;

  if (aSign.sign === Sign.Minus && bSign.sign === Sign.Plus) {
    // -A +B → U: A ⊆ B
    return { tag: "U", from: aKey, to: bKey, proof: proofName };
  }
  if (aSign.sign === Sign.Minus && bSign.sign === Sign.Minus) {
    // -A -B → E: A ⊥ B
    return { tag: "E", term1: aKey, term2: bKey, proof: proofName };
  }
  if (aSign.sign === Sign.Plus && bSign.sign === Sign.Plus) {
    // +A +B → I: A ∩ B ≠ ∅
    return { tag: "I", term1: aKey, term2: bKey, proof: proofName };
  }
  if (aSign.sign === Sign.Plus && bSign.sign === Sign.Minus) {
    // +A -B → O: not all A are B
    return { tag: "O", from: aKey, to: bKey, proof: proofName };
  }
  return null;
}

// ---------------------------------------------------------------------------
// Build term tree
// ---------------------------------------------------------------------------

export function buildTermTree(
  proofs: readonly { name: string; conclusion: Statement }[],
): TermTreeData {
  const uEdges: UEdge[] = [];
  const eEdges: EEdge[] = [];
  const iEdges: IEdge[] = [];
  const oEdges: OEdge[] = [];
  const termSet = new Set<string>();

  for (const { name, conclusion } of proofs) {
    const rel = extractRelation(conclusion, name);
    if (rel === null) continue;

    switch (rel.tag) {
      case "U":
        termSet.add(rel.from);
        termSet.add(rel.to);
        uEdges.push(rel);
        break;
      case "E":
        termSet.add(rel.term1);
        termSet.add(rel.term2);
        eEdges.push(rel);
        break;
      case "I":
        termSet.add(rel.term1);
        termSet.add(rel.term2);
        iEdges.push(rel);
        break;
      case "O":
        termSet.add(rel.from);
        termSet.add(rel.to);
        oEdges.push(rel);
        break;
    }
  }

  // Compute transitive closure of U-edges
  const uClosure = transitiveUClosure(uEdges, termSet);

  // Find N-pairs (complementation): if both X and non-X appear
  const nPairs: [string, string][] = [];
  for (const t of termSet) {
    if (!t.startsWith("non-")) {
      const comp = "non-" + t;
      if (termSet.has(comp)) {
        nPairs.push([t, comp]);
      }
    }
  }

  const terms = Array.from(termSet).sort();

  return { terms, uEdges, eEdges, iEdges, oEdges, uClosure, nPairs };
}

/** Compute transitive closure of U-edges (derived containment). */
function transitiveUClosure(
  edges: readonly UEdge[],
  terms: Set<string>,
): UEdge[] {
  // Build adjacency map
  const adj = new Map<string, Set<string>>();
  for (const t of terms) adj.set(t, new Set());
  for (const e of edges) {
    adj.get(e.from)!.add(e.to);
  }

  // Floyd-Warshall for transitive closure
  const termArr = Array.from(terms);
  const reach = new Map<string, Set<string>>();
  for (const t of termArr) {
    reach.set(t, new Set(adj.get(t)!));
  }

  for (const k of termArr) {
    for (const i of termArr) {
      if (!reach.get(i)!.has(k)) continue;
      for (const j of termArr) {
        if (reach.get(k)!.has(j)) {
          reach.get(i)!.add(j);
        }
      }
    }
  }

  // Derived edges = closure minus direct edges
  const directSet = new Set(edges.map((e) => `${e.from}→${e.to}`));
  const derived: UEdge[] = [];
  for (const [from, tos] of reach) {
    for (const to of tos) {
      if (from !== to && !directSet.has(`${from}→${to}`)) {
        derived.push({ tag: "U", from, to, proof: "(derived)" });
      }
    }
  }
  return derived;
}

// ---------------------------------------------------------------------------
// SVG renderer
// ---------------------------------------------------------------------------

const H_GAP = 30;
const V_GAP = 60;
const PAD = 20;

type NodeLayout = {
  key: string;
  x: number;
  y: number;
  w: number;
};

/**
 * Layout terms in a tree hierarchy based on U-relations.
 * Root nodes are those with no incoming U-edges.
 * Children of a node are terms that are U-related from it.
 */
function layoutTermTree(tree: TermTreeData): {
  nodes: NodeLayout[];
  width: number;
  height: number;
} {
  // Build adjacency: from → [to] for U-edges (direct only)
  const children = new Map<string, string[]>();
  const hasParent = new Set<string>();
  for (const t of tree.terms) children.set(t, []);

  for (const e of tree.uEdges) {
    children.get(e.from)!.push(e.to);
    hasParent.add(e.to);
  }

  // Roots = terms with no parent U-edge
  const roots = tree.terms.filter((t) => !hasParent.has(t));

  // BFS layout: assign levels and positions
  const levels = new Map<string, number>();
  const queue: string[] = [...roots];
  for (const r of roots) levels.set(r, 0);

  // Handle cycles: visited tracking
  const visited = new Set<string>();
  while (queue.length > 0) {
    const t = queue.shift()!;
    if (visited.has(t)) continue;
    visited.add(t);
    const lvl = levels.get(t)!;
    for (const child of children.get(t) ?? []) {
      if (!levels.has(child) || levels.get(child)! < lvl + 1) {
        levels.set(child, lvl + 1);
      }
      queue.push(child);
    }
  }

  // Terms not reached (isolated): put at level 0
  for (const t of tree.terms) {
    if (!levels.has(t)) levels.set(t, 0);
  }

  // Group by level
  const maxLevel = Math.max(0, ...Array.from(levels.values()));
  const byLevel: string[][] = Array.from({ length: maxLevel + 1 }, () => []);
  for (const t of tree.terms) {
    byLevel[levels.get(t)!].push(t);
  }

  // Assign x,y positions
  const nodes: NodeLayout[] = [];
  let maxWidth = 0;
  for (let lvl = 0; lvl <= maxLevel; lvl++) {
    const row = byLevel[lvl];
    // Compute row width with variable node widths
    let rowWidth = 0;
    for (let i = 0; i < row.length; i++) {
      rowWidth += nodeWidth(row[i]);
      if (i > 0) rowWidth += H_GAP;
    }
    maxWidth = Math.max(maxWidth, rowWidth);
    let xCursor = PAD;
    for (let i = 0; i < row.length; i++) {
      const w = nodeWidth(row[i]);
      nodes.push({
        key: row[i],
        x: xCursor,
        y: PAD + lvl * (NODE_H + V_GAP),
        w,
      });
      xCursor += w + H_GAP;
    }
  }

  return {
    nodes,
    width: maxWidth + 2 * PAD,
    height: PAD + (maxLevel + 1) * (NODE_H + V_GAP),
  };
}

/** Render the term tree as an interactive DOM element. */
export function renderTermTree(tree: TermTreeData): HTMLElement {
  const { nodes, width, height } = layoutTermTree(tree);
  const nodeMap = new Map<string, NodeLayout>();
  for (const n of nodes) nodeMap.set(n.key, n);

  const container = document.createElement("div");
  container.style.position = "relative";

  const svg = createSvg(width, height);
  appendBackground(svg, width, height);

  // Draw U-edges (direct) as solid arrows
  for (const e of tree.uEdges) {
    const from = nodeMap.get(e.from);
    const to = nodeMap.get(e.to);
    if (from && to) {
      drawNodeEdge(svg, from, to, "#4ec9b0", { arrow: true });
    }
  }

  // Draw transitive U-closure as dashed arrows
  for (const e of tree.uClosure) {
    const from = nodeMap.get(e.from);
    const to = nodeMap.get(e.to);
    if (from && to) {
      drawNodeEdge(svg, from, to, "#4ec9b0", { arrow: true, dashed: true, opacity: 0.5 });
    }
  }

  // Draw E-edges as red dashed lines
  for (const e of tree.eEdges) {
    const n1 = nodeMap.get(e.term1);
    const n2 = nodeMap.get(e.term2);
    if (n1 && n2) {
      drawNodeEdge(svg, n1, n2, "#f44747", { dashed: true, opacity: 0.6 });
    }
  }

  // Draw I-edges as blue dotted lines
  for (const e of tree.iEdges) {
    const n1 = nodeMap.get(e.term1);
    const n2 = nodeMap.get(e.term2);
    if (n1 && n2) {
      drawNodeEdge(svg, n1, n2, "#569cd6", { dashed: true, opacity: 0.6 });
    }
  }

  // Draw N-pairs as purple lines
  for (const [pos, neg] of tree.nPairs) {
    const n1 = nodeMap.get(pos);
    const n2 = nodeMap.get(neg);
    if (n1 && n2) {
      drawNodeEdge(svg, n1, n2, "#c586c0", { opacity: 0.6 });
    }
  }

  // Draw nodes
  for (const n of nodes) {
    drawNode(svg, n.x, n.y, n.w, n.key);
  }

  // Legend
  const legendY = height - 16;
  drawLegend(
    svg,
    [
      { color: "#4ec9b0", label: "U (containment)", dash: false },
      { color: "#f44747", label: "E (disjoint)", dash: true },
      { color: "#569cd6", label: "I (overlap)", dash: true },
      { color: "#c586c0", label: "N (complement)", dash: false },
    ],
    PAD,
    legendY,
  );

  container.appendChild(svg);
  return container;
}
