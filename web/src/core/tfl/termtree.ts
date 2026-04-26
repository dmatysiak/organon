// Englebretsen term tree — visualizes U/N/E/I relations between terms
// Reference: Englebretsen, "Robust Reality" (2012)

import { Sign, type Term, type SignedTerm, type Statement } from "./types";

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

const NODE_W = 90;
const NODE_H = 28;
const H_GAP = 30;
const V_GAP = 60;
const PAD = 20;

type NodeLayout = {
  key: string;
  x: number;
  y: number;
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
    const rowWidth = row.length * NODE_W + (row.length - 1) * H_GAP;
    maxWidth = Math.max(maxWidth, rowWidth);
    for (let i = 0; i < row.length; i++) {
      nodes.push({
        key: row[i],
        x: PAD + i * (NODE_W + H_GAP),
        y: PAD + lvl * (NODE_H + V_GAP),
      });
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

  const NS = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(NS, "svg");
  svg.setAttribute("width", String(width));
  svg.setAttribute("height", String(height));
  svg.setAttribute("font-family", "'SF Mono','Fira Code',Menlo,monospace");
  svg.setAttribute("font-size", "12");

  // Background
  const bg = document.createElementNS(NS, "rect");
  bg.setAttribute("width", String(width));
  bg.setAttribute("height", String(height));
  bg.setAttribute("fill", "#1e1e1e");
  bg.setAttribute("rx", "4");
  svg.appendChild(bg);

  // Draw U-edges (direct) as solid arrows
  for (const e of tree.uEdges) {
    const from = nodeMap.get(e.from);
    const to = nodeMap.get(e.to);
    if (from && to) {
      drawArrow(svg, NS, from, to, "#4ec9b0", "U", false);
    }
  }

  // Draw transitive U-closure as dashed arrows
  for (const e of tree.uClosure) {
    const from = nodeMap.get(e.from);
    const to = nodeMap.get(e.to);
    if (from && to) {
      drawArrow(svg, NS, from, to, "#4ec9b0", "U*", true);
    }
  }

  // Draw E-edges as red dashed lines
  for (const e of tree.eEdges) {
    const n1 = nodeMap.get(e.term1);
    const n2 = nodeMap.get(e.term2);
    if (n1 && n2) {
      drawLine(svg, NS, n1, n2, "#f44747", "E", true);
    }
  }

  // Draw I-edges as blue dotted lines
  for (const e of tree.iEdges) {
    const n1 = nodeMap.get(e.term1);
    const n2 = nodeMap.get(e.term2);
    if (n1 && n2) {
      drawLine(svg, NS, n1, n2, "#569cd6", "I", true);
    }
  }

  // Draw N-pairs as double-headed purple arrows
  for (const [pos, neg] of tree.nPairs) {
    const n1 = nodeMap.get(pos);
    const n2 = nodeMap.get(neg);
    if (n1 && n2) {
      drawLine(svg, NS, n1, n2, "#c586c0", "N", false);
    }
  }

  // Draw nodes
  for (const n of nodes) {
    const g = document.createElementNS(NS, "g");
    g.style.cursor = "default";

    const rect = document.createElementNS(NS, "rect");
    rect.setAttribute("x", String(n.x));
    rect.setAttribute("y", String(n.y));
    rect.setAttribute("width", String(NODE_W));
    rect.setAttribute("height", String(NODE_H));
    rect.setAttribute("rx", "4");
    rect.setAttribute("fill", "#264f78");
    rect.setAttribute("stroke", "#3c3c3c");
    g.appendChild(rect);

    const text = document.createElementNS(NS, "text");
    text.setAttribute("x", String(n.x + NODE_W / 2));
    text.setAttribute("y", String(n.y + NODE_H / 2 + 4));
    text.setAttribute("text-anchor", "middle");
    text.setAttribute("fill", "#d4d4d4");
    text.textContent = n.key;
    g.appendChild(text);

    svg.appendChild(g);
  }

  // Legend
  const legendY = height - 16;
  const legendItems = [
    { color: "#4ec9b0", label: "U (containment)", dash: false },
    { color: "#f44747", label: "E (disjoint)", dash: true },
    { color: "#569cd6", label: "I (overlap)", dash: true },
    { color: "#c586c0", label: "N (complement)", dash: false },
  ];
  let lx = PAD;
  for (const item of legendItems) {
    const line = document.createElementNS(NS, "line");
    line.setAttribute("x1", String(lx));
    line.setAttribute("y1", String(legendY));
    line.setAttribute("x2", String(lx + 20));
    line.setAttribute("y2", String(legendY));
    line.setAttribute("stroke", item.color);
    line.setAttribute("stroke-width", "2");
    if (item.dash) line.setAttribute("stroke-dasharray", "4,3");
    svg.appendChild(line);
    const lbl = document.createElementNS(NS, "text");
    lbl.setAttribute("x", String(lx + 24));
    lbl.setAttribute("y", String(legendY + 4));
    lbl.setAttribute("fill", "#969696");
    lbl.setAttribute("font-size", "10");
    lbl.textContent = item.label;
    svg.appendChild(lbl);
    lx += 24 + item.label.length * 6 + 16;
  }

  container.appendChild(svg);
  return container;
}

// ---------------------------------------------------------------------------
// SVG drawing helpers
// ---------------------------------------------------------------------------

function drawArrow(
  svg: SVGSVGElement,
  NS: string,
  from: NodeLayout,
  to: NodeLayout,
  color: string,
  _label: string,
  dashed: boolean,
): void {
  const x1 = from.x + NODE_W / 2;
  const y1 = from.y + NODE_H;
  const x2 = to.x + NODE_W / 2;
  const y2 = to.y;

  const line = document.createElementNS(NS, "line");
  line.setAttribute("x1", String(x1));
  line.setAttribute("y1", String(y1));
  line.setAttribute("x2", String(x2));
  line.setAttribute("y2", String(y2));
  line.setAttribute("stroke", color);
  line.setAttribute("stroke-width", "1.5");
  if (dashed) line.setAttribute("stroke-dasharray", "4,3");
  line.setAttribute("opacity", dashed ? "0.5" : "0.8");
  svg.appendChild(line);

  // Arrowhead
  const angle = Math.atan2(y2 - y1, x2 - x1);
  const headLen = 8;
  const ax = x2 - headLen * Math.cos(angle - Math.PI / 6);
  const ay = y2 - headLen * Math.sin(angle - Math.PI / 6);
  const bx = x2 - headLen * Math.cos(angle + Math.PI / 6);
  const by = y2 - headLen * Math.sin(angle + Math.PI / 6);
  const poly = document.createElementNS(NS, "polygon");
  poly.setAttribute("points", `${x2},${y2} ${ax},${ay} ${bx},${by}`);
  poly.setAttribute("fill", color);
  poly.setAttribute("opacity", dashed ? "0.5" : "0.8");
  svg.appendChild(poly);
}

function drawLine(
  svg: SVGSVGElement,
  NS: string,
  n1: NodeLayout,
  n2: NodeLayout,
  color: string,
  _label: string,
  dashed: boolean,
): void {
  // Connect sides or top/bottom depending on relative position
  const x1 = n1.x + NODE_W / 2;
  const y1 = n1.y + NODE_H / 2;
  const x2 = n2.x + NODE_W / 2;
  const y2 = n2.y + NODE_H / 2;

  const line = document.createElementNS(NS, "line");
  line.setAttribute("x1", String(x1));
  line.setAttribute("y1", String(y1));
  line.setAttribute("x2", String(x2));
  line.setAttribute("y2", String(y2));
  line.setAttribute("stroke", color);
  line.setAttribute("stroke-width", "1.5");
  if (dashed) line.setAttribute("stroke-dasharray", "4,3");
  line.setAttribute("opacity", "0.6");
  svg.appendChild(line);
}
