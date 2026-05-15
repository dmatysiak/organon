// Tree of Predicability — Sommers/Englebretsen ontological category tree
//
// Two terms are predicability-connected when they co-occur in any
// statement (premise or conclusion). The Sommers thesis says that
// natural-language terms form a *tree* under this relation.
// Extra edges beyond the spanning tree indicate cross-category
// predication; disconnected components represent distinct
// ontological categories.

import type { Term, Statement } from "./types";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export type PredEdge = {
  readonly term1: string;
  readonly term2: string;
  /** Proof names where this co-occurrence was observed. */
  readonly proofs: readonly string[];
};

export type PredTreeData = {
  /** All unique term keys. */
  readonly terms: readonly string[];
  /** Spanning tree edges. */
  readonly treeEdges: readonly PredEdge[];
  /** Extra edges beyond the spanning tree (cycle-forming). */
  readonly extraEdges: readonly PredEdge[];
  /** Connected components (each is a sorted list of term keys). */
  readonly components: readonly (readonly string[])[];
};

// ---------------------------------------------------------------------------
// Build
// ---------------------------------------------------------------------------

function termKey(t: Term): string {
  return t.complemented ? "non-" + t.termName : t.termName;
}

/** Extract all atomic term keys from a statement. */
function statementTerms(stmt: Statement): string[] {
  const keys: string[] = [];
  for (const st of stmt.terms) {
    if (st.termExpr.tag === "Atomic") {
      keys.push(termKey(st.termExpr.term));
    } else {
      // Compound: collect inner atomic terms
      for (const inner of st.termExpr.elements) {
        if (inner.termExpr.tag === "Atomic") {
          keys.push(termKey(inner.termExpr.term));
        }
      }
    }
  }
  return keys;
}

/** Canonical key for an undirected edge. */
function edgeKey(a: string, b: string): string {
  return a < b ? `${a}||${b}` : `${b}||${a}`;
}

export function buildPredTree(
  proofs: readonly {
    name: string;
    premises: readonly Statement[];
    conclusion: Statement;
  }[],
): PredTreeData {
  const termSet = new Set<string>();
  // Map from edgeKey → { term1, term2, proofs }
  const edgeMap = new Map<
    string,
    { term1: string; term2: string; proofs: Set<string> }
  >();

  for (const { name, premises, conclusion } of proofs) {
    const allStmts = [...premises, conclusion];
    for (const stmt of allStmts) {
      const terms = statementTerms(stmt);
      for (const t of terms) termSet.add(t);
      // Every pair of distinct terms in the same statement is connected
      for (let i = 0; i < terms.length; i++) {
        for (let j = i + 1; j < terms.length; j++) {
          const a = terms[i];
          const b = terms[j];
          if (a === b) continue;
          const key = edgeKey(a, b);
          const existing = edgeMap.get(key);
          if (existing) {
            existing.proofs.add(name);
          } else {
            const [t1, t2] = a < b ? [a, b] : [b, a];
            edgeMap.set(key, { term1: t1, term2: t2, proofs: new Set([name]) });
          }
        }
      }
    }
  }

  const terms = Array.from(termSet).sort();
  const allEdges: PredEdge[] = Array.from(edgeMap.values()).map((e) => ({
    term1: e.term1,
    term2: e.term2,
    proofs: Array.from(e.proofs).sort(),
  }));

  // Compute spanning forest via BFS (starting from highest-degree nodes)
  const adj = new Map<string, Set<string>>();
  for (const t of terms) adj.set(t, new Set());
  for (const e of allEdges) {
    adj.get(e.term1)!.add(e.term2);
    adj.get(e.term2)!.add(e.term1);
  }

  // Sort terms by degree descending for root selection
  const byDegree = [...terms].sort(
    (a, b) => (adj.get(b)?.size ?? 0) - (adj.get(a)?.size ?? 0),
  );

  const visited = new Set<string>();
  const treeEdgeKeys = new Set<string>();
  const treeEdges: PredEdge[] = [];
  const components: string[][] = [];

  for (const root of byDegree) {
    if (visited.has(root)) continue;
    const component: string[] = [];
    const queue = [root];
    visited.add(root);
    while (queue.length > 0) {
      const node = queue.shift()!;
      component.push(node);
      for (const neighbor of adj.get(node)!) {
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          queue.push(neighbor);
          const key = edgeKey(node, neighbor);
          treeEdgeKeys.add(key);
          const edge = edgeMap.get(key)!;
          treeEdges.push({
            term1: edge.term1,
            term2: edge.term2,
            proofs: Array.from(edge.proofs).sort(),
          });
        }
      }
    }
    component.sort();
    components.push(component);
  }

  // Handle isolated terms (no edges)
  for (const t of terms) {
    if (!visited.has(t)) {
      components.push([t]);
    }
  }

  const extraEdges = allEdges.filter(
    (e) => !treeEdgeKeys.has(edgeKey(e.term1, e.term2)),
  );

  return { terms, treeEdges, extraEdges, components };
}

// ---------------------------------------------------------------------------
// SVG renderer
// ---------------------------------------------------------------------------

const NODE_H = 28;
const CHAR_W = 7.2; // approximate monospace char width at 12px
const NODE_PAD_X = 16; // horizontal padding inside node
const H_GAP = 24;
const V_GAP = 56;
const PAD = 24;
const LEGEND_H = 28;

type NodeLayout = {
  key: string;
  x: number;
  y: number;
  w: number;
  component: number;
};

// Palette for distinct components
const COMPONENT_COLORS = [
  "#4ec9b0",
  "#569cd6",
  "#c586c0",
  "#dcdcaa",
  "#d7ba7d",
  "#ce9178",
  "#9cdcfe",
  "#b5cea8",
];

function componentColor(idx: number): string {
  return COMPONENT_COLORS[idx % COMPONENT_COLORS.length];
}

function nodeWidth(key: string): number {
  return Math.max(40, key.length * CHAR_W + NODE_PAD_X);
}

function layoutPredTree(tree: PredTreeData): {
  nodes: NodeLayout[];
  width: number;
  height: number;
} {
  // Build adjacency from tree edges only (for layout)
  const children = new Map<string, string[]>();
  for (const t of tree.terms) children.set(t, []);

  // Build undirected adjacency for tree edges
  const treeAdj = new Map<string, Set<string>>();
  for (const t of tree.terms) treeAdj.set(t, new Set());
  for (const e of tree.treeEdges) {
    treeAdj.get(e.term1)!.add(e.term2);
    treeAdj.get(e.term2)!.add(e.term1);
  }

  // For each component, pick the highest-degree node as root, BFS to assign levels
  const levels = new Map<string, number>();
  const parentOf = new Map<string, string | null>();
  const compIndex = new Map<string, number>();

  for (let ci = 0; ci < tree.components.length; ci++) {
    const comp = tree.components[ci];
    for (const t of comp) compIndex.set(t, ci);

    // Pick root: highest degree within this component's tree edges
    let root = comp[0];
    let maxDeg = 0;
    for (const t of comp) {
      const deg = treeAdj.get(t)?.size ?? 0;
      if (deg > maxDeg) {
        maxDeg = deg;
        root = t;
      }
    }

    // BFS
    const visited = new Set<string>();
    const queue = [root];
    visited.add(root);
    levels.set(root, 0);
    parentOf.set(root, null);

    while (queue.length > 0) {
      const node = queue.shift()!;
      const lvl = levels.get(node)!;
      for (const neighbor of treeAdj.get(node)!) {
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          levels.set(neighbor, lvl + 1);
          parentOf.set(neighbor, node);
          children.get(node)!.push(neighbor);
          queue.push(neighbor);
        }
      }
    }
  }

  // Assign x positions using absolute coordinates
  // Each leaf gets placed after the previous; internal nodes center over children
  const nodeX = new Map<string, number>();
  let nextX = 0;

  function assignPositions(node: string): void {
    const ch = children.get(node) ?? [];
    if (ch.length === 0) {
      nodeX.set(node, nextX);
      nextX += nodeWidth(node) + H_GAP;
      return;
    }
    for (const c of ch) assignPositions(c);
    const firstX = nodeX.get(ch[0])!;
    const lastX = nodeX.get(ch[ch.length - 1])!;
    const firstW = nodeWidth(ch[0]);
    const lastW = nodeWidth(ch[ch.length - 1]);
    const myW = nodeWidth(node);
    const center = (firstX + firstW / 2 + lastX + lastW / 2) / 2 - myW / 2;
    nodeX.set(node, center);
  }

  // Lay out each component vertically stacked, each with its own y-offset
  const COMP_GAP = 24; // vertical gap between components
  const compYOffset: number[] = [];
  const compMaxLevel: number[] = [];
  let currentY = PAD;

  for (let ci = 0; ci < tree.components.length; ci++) {
    const comp = tree.components[ci];
    // Reset x counter for each component so they all start at x=0
    nextX = 0;
    const root = comp.find((t) => levels.get(t) === 0 || !parentOf.has(t));
    if (root !== undefined) {
      assignPositions(root);
    }

    // Find max level within this component
    let maxLvl = 0;
    for (const t of comp) {
      const lvl = levels.get(t) ?? 0;
      if (lvl > maxLvl) maxLvl = lvl;
    }
    compMaxLevel.push(maxLvl);
    compYOffset.push(currentY);
    currentY += (maxLvl + 1) * (NODE_H + V_GAP) + COMP_GAP;
  }

  const nodes: NodeLayout[] = [];
  for (const t of tree.terms) {
    const ci = compIndex.get(t) ?? 0;
    const x = nodeX.get(t) ?? 0;
    const lvl = levels.get(t) ?? 0;
    nodes.push({
      key: t,
      x: PAD + x,
      y: compYOffset[ci] + lvl * (NODE_H + V_GAP),
      w: nodeWidth(t),
      component: ci,
    });
  }

  const maxX = Math.max(0, ...nodes.map((n) => n.x + n.w));
  const totalHeight = currentY - COMP_GAP + LEGEND_H + PAD;
  return {
    nodes,
    width: maxX + PAD,
    height: totalHeight,
  };
}

export function renderPredTree(tree: PredTreeData): HTMLElement {
  if (tree.terms.length === 0) {
    const div = document.createElement("div");
    div.textContent = "No predicability relations found.";
    return div;
  }

  const { nodes, width, height } = layoutPredTree(tree);
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

  // Draw spanning tree edges (solid lines, component-colored)
  for (const e of tree.treeEdges) {
    const n1 = nodeMap.get(e.term1);
    const n2 = nodeMap.get(e.term2);
    if (n1 && n2) {
      const color = componentColor(n1.component);
      drawEdge(svg, NS, n1, n2, color, false, 0.8);
    }
  }

  // Draw extra edges (dashed orange — violations of the tree thesis)
  for (const e of tree.extraEdges) {
    const n1 = nodeMap.get(e.term1);
    const n2 = nodeMap.get(e.term2);
    if (n1 && n2) {
      drawEdge(svg, NS, n1, n2, "#f0a500", true, 0.5);
    }
  }

  // Draw nodes
  for (const n of nodes) {
    const color = componentColor(n.component);

    const g = document.createElementNS(NS, "g");
    g.style.cursor = "default";

    const rect = document.createElementNS(NS, "rect");
    rect.setAttribute("x", String(n.x));
    rect.setAttribute("y", String(n.y));
    rect.setAttribute("width", String(n.w));
    rect.setAttribute("height", String(NODE_H));
    rect.setAttribute("rx", "4");
    rect.setAttribute("fill", "#264f78");
    rect.setAttribute("stroke", color);
    rect.setAttribute("stroke-width", "1.5");
    g.appendChild(rect);

    const text = document.createElementNS(NS, "text");
    text.setAttribute("x", String(n.x + n.w / 2));
    text.setAttribute("y", String(n.y + NODE_H / 2 + 4));
    text.setAttribute("text-anchor", "middle");
    text.setAttribute("fill", "#d4d4d4");
    text.textContent = n.key;
    g.appendChild(text);

    // Tooltip with component index
    const title = document.createElementNS(NS, "title");
    title.textContent = `${n.key} — category ${n.component + 1}`;
    g.appendChild(title);

    svg.appendChild(g);
  }

  // Legend
  const legendY = height - 14;
  const legendItems: { color: string; label: string; dash: boolean }[] = [];

  // Show one entry per component
  const numComp = tree.components.length;
  if (numComp > 1) {
    for (let i = 0; i < Math.min(numComp, 4); i++) {
      legendItems.push({
        color: componentColor(i),
        label: `category ${i + 1}`,
        dash: false,
      });
    }
    if (numComp > 4) {
      legendItems.push({
        color: "#969696",
        label: `+${numComp - 4} more`,
        dash: false,
      });
    }
  } else {
    legendItems.push({
      color: componentColor(0),
      label: "tree edge",
      dash: false,
    });
  }

  if (tree.extraEdges.length > 0) {
    legendItems.push({
      color: "#f0a500",
      label: "extra (non-tree)",
      dash: true,
    });
  }

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

  // Component / edge summary
  const summaryY = legendY - 16;
  const summaryText = document.createElementNS(NS, "text");
  summaryText.setAttribute("x", String(PAD));
  summaryText.setAttribute("y", String(summaryY));
  summaryText.setAttribute("fill", "#969696");
  summaryText.setAttribute("font-size", "10");
  const parts: string[] = [
    `${tree.terms.length} terms`,
    `${numComp} ${numComp === 1 ? "category" : "categories"}`,
  ];
  if (tree.extraEdges.length > 0) {
    parts.push(
      `${tree.extraEdges.length} extra ${tree.extraEdges.length === 1 ? "edge" : "edges"}`,
    );
  }
  summaryText.textContent = parts.join(" · ");
  svg.appendChild(summaryText);

  container.appendChild(svg);
  return container;
}

// ---------------------------------------------------------------------------
// SVG helpers
// ---------------------------------------------------------------------------

function drawEdge(
  svg: SVGSVGElement,
  NS: string,
  n1: NodeLayout,
  n2: NodeLayout,
  color: string,
  dashed: boolean,
  opacity: number,
): void {
  // Connect bottom of higher node to top of lower node (if different levels)
  // or center-to-center if on same level
  const sameLvl = n1.y === n2.y;
  let x1: number, y1: number, x2: number, y2: number;

  if (sameLvl) {
    x1 = n1.x + n1.w / 2;
    y1 = n1.y + NODE_H / 2;
    x2 = n2.x + n2.w / 2;
    y2 = n2.y + NODE_H / 2;
  } else {
    const [upper, lower] = n1.y < n2.y ? [n1, n2] : [n2, n1];
    x1 = upper.x + upper.w / 2;
    y1 = upper.y + NODE_H;
    x2 = lower.x + lower.w / 2;
    y2 = lower.y;
  }

  const line = document.createElementNS(NS, "line");
  line.setAttribute("x1", String(x1));
  line.setAttribute("y1", String(y1));
  line.setAttribute("x2", String(x2));
  line.setAttribute("y2", String(y2));
  line.setAttribute("stroke", color);
  line.setAttribute("stroke-width", "1.5");
  if (dashed) line.setAttribute("stroke-dasharray", "5,3");
  line.setAttribute("opacity", String(opacity));
  svg.appendChild(line);
}
