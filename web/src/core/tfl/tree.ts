// Term tree — SVG visualization of TFL cancellation

import { Sign, term, type TermExpr, termExprEq } from "./types";
import type { SignedTerm, Statement, Inference } from "./types";
import type { Cancellation, CancelledPair, ValidationResult } from "./validity";
import { cancellation, validate } from "./validity";
import { prettySignedTerm } from "./pretty";

// ---------------------------------------------------------------------------
// Data model
// ---------------------------------------------------------------------------

export type TermNode = {
  /** Premise index (0-based). */
  readonly premiseIndex: number;
  /** Position within the premise's terms (0-based). */
  readonly termIndex: number;
  /** The signed term itself. */
  readonly signedTerm: SignedTerm;
  /** Whether this term was cancelled. */
  readonly cancelled: boolean;
  /** Index into CancelledPair array, or -1 if uncancelled. */
  readonly pairIndex: number;
};

export type CancelEdge = {
  readonly pairIndex: number;
  readonly node1: [number, number]; // [premiseIndex, termIndex]
  readonly node2: [number, number];
};

export type TreeData = {
  readonly premises: readonly Statement[];
  readonly conclusion: Statement;
  readonly nodes: readonly TermNode[];
  readonly edges: readonly CancelEdge[];
  readonly uncancelledNodes: readonly TermNode[];
  readonly valid: boolean;
};

// ---------------------------------------------------------------------------
// Build tree from an inference + cancellation
// ---------------------------------------------------------------------------

export function buildTree(inf: Inference, result: ValidationResult): TreeData {
  const cancel = result.cancellation;

  // Build nodes for all premise terms
  const nodes: TermNode[] = [];
  for (let pi = 0; pi < inf.premises.length; pi++) {
    const stmt = inf.premises[pi];
    for (let ti = 0; ti < stmt.terms.length; ti++) {
      const st = stmt.terms[ti];
      const pairIdx = findPairIndex(cancel, pi, st);
      nodes.push({
        premiseIndex: pi,
        termIndex: ti,
        signedTerm: st,
        cancelled: pairIdx >= 0,
        pairIndex: pairIdx,
      });
    }
  }

  // Build edges from cancelled pairs
  const edges: CancelEdge[] = [];
  for (let ci = 0; ci < cancel.cancelled.length; ci++) {
    const pair = cancel.cancelled[ci];
    const n1 = findNode(nodes, pair.premiseIndex1, pair);
    const n2 = findNode(nodes, pair.premiseIndex2, pair);
    if (n1 && n2) {
      edges.push({
        pairIndex: ci,
        node1: [n1.premiseIndex, n1.termIndex],
        node2: [n2.premiseIndex, n2.termIndex],
      });
    }
  }

  const uncancelledNodes = nodes.filter((n) => !n.cancelled);

  return {
    premises: inf.premises,
    conclusion: inf.conclusion,
    nodes,
    edges,
    uncancelledNodes,
    valid: result.tag === "Valid",
  };
}

function findPairIndex(
  cancel: Cancellation,
  premIdx: number,
  st: SignedTerm,
): number {
  for (let i = 0; i < cancel.cancelled.length; i++) {
    const p = cancel.cancelled[i];
    if (
      (p.premiseIndex1 === premIdx || p.premiseIndex2 === premIdx) &&
      termExprEq(p.cancelledTermExpr, st.termExpr) &&
      arrEq(p.cancelledPositions as number[], st.positions as number[])
    ) {
      return i;
    }
  }
  return -1;
}

function findNode(
  nodes: TermNode[],
  premIdx: number,
  pair: CancelledPair,
): TermNode | undefined {
  return nodes.find(
    (n) =>
      n.premiseIndex === premIdx &&
      termExprEq(n.signedTerm.termExpr, pair.cancelledTermExpr) &&
      arrEq(
        n.signedTerm.positions as number[],
        pair.cancelledPositions as number[],
      ) &&
      n.pairIndex >= 0,
  );
}

function arrEq(a: number[], b: number[]): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

// ---------------------------------------------------------------------------
// SVG renderer
// ---------------------------------------------------------------------------

const NODE_W = 80;
const NODE_H = 32;
const H_GAP = 16;
const V_GAP = 48;
const PAD = 16;
const CONCL_GAP = 24;

type Rect = { x: number; y: number; w: number; h: number };

type LayoutItem = { label: string; rect: Rect; node: TermNode };
type ConclItem = { label: string; rect: Rect };

function layoutTree(tree: TreeData): {
  premiseRows: LayoutItem[][];
  conclItems: ConclItem[];
  nodeRect: Map<string, Rect>;
  svgW: number;
  svgH: number;
  conclusionY: number;
} {
  const measure = (s: string): number => Math.max(NODE_W, s.length * 8 + 16);

  const premiseRows: LayoutItem[][] = [];
  let maxRowWidth = 0;

  for (let pi = 0; pi < tree.premises.length; pi++) {
    const row: LayoutItem[] = [];
    let x = PAD;
    const rowNodes = tree.nodes.filter((n) => n.premiseIndex === pi);
    for (const n of rowNodes) {
      const label = prettySignedTerm(n.signedTerm);
      const w = measure(label);
      row.push({ label, rect: { x, y: 0, w, h: NODE_H }, node: n });
      x += w + H_GAP;
    }
    maxRowWidth = Math.max(maxRowWidth, x - H_GAP + PAD);
    premiseRows.push(row);
  }

  for (let pi = 0; pi < premiseRows.length; pi++) {
    const y = PAD + pi * (NODE_H + V_GAP);
    for (const item of premiseRows[pi]) {
      item.rect.y = y;
    }
  }

  const conclusionY = PAD + premiseRows.length * (NODE_H + V_GAP) + CONCL_GAP;

  const conclItems: ConclItem[] = [];
  {
    let x = PAD;
    for (const st of tree.conclusion.terms) {
      const label = prettySignedTerm(st);
      const w = measure(label);
      conclItems.push({ label, rect: { x, y: conclusionY, w, h: NODE_H } });
      x += w + H_GAP;
    }
    maxRowWidth = Math.max(maxRowWidth, x - H_GAP + PAD);
  }

  const nodeRect = new Map<string, Rect>();
  for (const row of premiseRows) {
    for (const item of row) {
      nodeRect.set(
        `${item.node.premiseIndex},${item.node.termIndex}`,
        item.rect,
      );
    }
  }

  return {
    premiseRows,
    conclItems,
    nodeRect,
    svgW: maxRowWidth,
    svgH: conclusionY + NODE_H + PAD,
    conclusionY,
  };
}

/** Render the tree as a raw SVG string (for static contexts / export). */
export function renderTreeSvg(tree: TreeData): string {
  const { premiseRows, conclItems, nodeRect, svgW, svgH, conclusionY } =
    layoutTree(tree);

  const parts: string[] = [];
  parts.push(
    `<svg xmlns="http://www.w3.org/2000/svg" width="${svgW}" height="${svgH}" ` +
      `font-family="'SF Mono','Fira Code',Menlo,monospace" font-size="13">`,
  );
  parts.push(`<rect width="${svgW}" height="${svgH}" fill="#1e1e1e" rx="4"/>`);

  emitEdges(parts, tree, nodeRect);
  emitPremiseNodes(parts, premiseRows);
  emitFlowLines(parts, tree, nodeRect, conclItems);
  emitThereforeLine(parts, svgW, conclusionY);
  emitConclusionNodes(parts, conclItems, tree.valid);

  parts.push("</svg>");
  return parts.join("\n");
}

/**
 * Render an interactive tree as a DOM element.
 *
 * - Hovering a cancelled node highlights its cancellation partner and their
 *   connecting edge.
 * - Clicking a node shows a tooltip with term details (sign, term name,
 *   positions, premise index, cancelled status).
 * - An "Export SVG" button is appended below the tree.
 */
export function renderTreeInteractive(tree: TreeData): HTMLElement {
  const { premiseRows, conclItems, nodeRect, svgW, svgH, conclusionY } =
    layoutTree(tree);

  const container = document.createElement("div");
  container.style.position = "relative";

  // Build SVG as DOM
  const NS = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(NS, "svg");
  svg.setAttribute("width", String(svgW));
  svg.setAttribute("height", String(svgH));
  svg.setAttribute("font-family", "'SF Mono','Fira Code',Menlo,monospace");
  svg.setAttribute("font-size", "13");

  const bg = document.createElementNS(NS, "rect");
  bg.setAttribute("width", String(svgW));
  bg.setAttribute("height", String(svgH));
  bg.setAttribute("fill", "#1e1e1e");
  bg.setAttribute("rx", "4");
  svg.appendChild(bg);

  // Edge elements keyed by pairIndex for highlighting
  const edgeEls = new Map<number, SVGLineElement>();
  for (const edge of tree.edges) {
    const r1 = nodeRect.get(`${edge.node1[0]},${edge.node1[1]}`);
    const r2 = nodeRect.get(`${edge.node2[0]},${edge.node2[1]}`);
    if (r1 && r2) {
      const line = document.createElementNS(NS, "line");
      line.setAttribute("x1", String(r1.x + r1.w / 2));
      line.setAttribute("y1", String(r1.y + r1.h));
      line.setAttribute("x2", String(r2.x + r2.w / 2));
      line.setAttribute("y2", String(r2.y));
      line.setAttribute("stroke", "#f44747");
      line.setAttribute("stroke-width", "1.5");
      line.setAttribute("stroke-dasharray", "4,3");
      line.setAttribute("opacity", "0.7");
      svg.appendChild(line);
      edgeEls.set(edge.pairIndex, line);
    }
  }

  // Node groups keyed by "premiseIndex,termIndex"
  const nodeGroups = new Map<string, SVGGElement>();

  // Premise nodes
  for (const row of premiseRows) {
    for (const item of row) {
      const { x, y, w, h } = item.rect;
      const n = item.node;
      const fill = n.cancelled ? "#2d2d2d" : "#264f78";
      const textColor = n.cancelled ? "#666" : "#4ec9b0";
      const key = `${n.premiseIndex},${n.termIndex}`;

      const g = document.createElementNS(NS, "g");
      g.setAttribute("data-pair", String(n.pairIndex));
      g.setAttribute("data-key", key);
      g.style.cursor = "pointer";

      const rect = document.createElementNS(NS, "rect");
      rect.setAttribute("x", String(x));
      rect.setAttribute("y", String(y));
      rect.setAttribute("width", String(w));
      rect.setAttribute("height", String(h));
      rect.setAttribute("rx", "4");
      rect.setAttribute("fill", fill);
      rect.setAttribute("stroke", "#3c3c3c");
      g.appendChild(rect);

      const text = document.createElementNS(NS, "text");
      text.setAttribute("x", String(x + w / 2));
      text.setAttribute("y", String(y + h / 2 + 4));
      text.setAttribute("text-anchor", "middle");
      text.setAttribute("fill", textColor);
      text.textContent = item.label;
      g.appendChild(text);

      if (n.cancelled) {
        const strike = document.createElementNS(NS, "line");
        strike.setAttribute("x1", String(x + 4));
        strike.setAttribute("y1", String(y + h / 2));
        strike.setAttribute("x2", String(x + w - 4));
        strike.setAttribute("y2", String(y + h / 2));
        strike.setAttribute("stroke", "#f44747");
        strike.setAttribute("stroke-width", "1.5");
        strike.setAttribute("opacity", "0.6");
        g.appendChild(strike);
      }

      svg.appendChild(g);
      nodeGroups.set(key, g);
    }
  }

  // Flow lines
  for (const un of tree.uncancelledNodes) {
    const r = nodeRect.get(`${un.premiseIndex},${un.termIndex}`);
    if (r) {
      const ci = conclItems.findIndex(
        (c) => c.label === prettySignedTerm(un.signedTerm),
      );
      if (ci >= 0) {
        const cr = conclItems[ci].rect;
        const line = document.createElementNS(NS, "line");
        line.setAttribute("x1", String(r.x + r.w / 2));
        line.setAttribute("y1", String(r.y + r.h));
        line.setAttribute("x2", String(cr.x + cr.w / 2));
        line.setAttribute("y2", String(cr.y));
        line.setAttribute("stroke", "#4ec9b0");
        line.setAttribute("stroke-width", "1.5");
        line.setAttribute("opacity", "0.5");
        svg.appendChild(line);
      }
    }
  }

  // Therefore line
  {
    const lineY = conclusionY - CONCL_GAP / 2;
    const tl = document.createElementNS(NS, "line");
    tl.setAttribute("x1", String(PAD));
    tl.setAttribute("y1", String(lineY));
    tl.setAttribute("x2", String(svgW - PAD));
    tl.setAttribute("y2", String(lineY));
    tl.setAttribute("stroke", "#569cd6");
    tl.setAttribute("stroke-width", "1");
    tl.setAttribute("stroke-dasharray", "6,4");
    svg.appendChild(tl);
    const sym = document.createElementNS(NS, "text");
    sym.setAttribute("x", String(PAD));
    sym.setAttribute("y", String(lineY - 4));
    sym.setAttribute("fill", "#569cd6");
    sym.setAttribute("font-size", "11");
    sym.textContent = "∴";
    svg.appendChild(sym);
  }

  // Conclusion nodes
  for (const item of conclItems) {
    const { x, y, w, h } = item.rect;
    const fill = tree.valid ? "#1b4332" : "#4a1919";
    const textColor = tree.valid ? "#4ec9b0" : "#f44747";
    const rect = document.createElementNS(NS, "rect");
    rect.setAttribute("x", String(x));
    rect.setAttribute("y", String(y));
    rect.setAttribute("width", String(w));
    rect.setAttribute("height", String(h));
    rect.setAttribute("rx", "4");
    rect.setAttribute("fill", fill);
    rect.setAttribute("stroke", "#3c3c3c");
    svg.appendChild(rect);
    const text = document.createElementNS(NS, "text");
    text.setAttribute("x", String(x + w / 2));
    text.setAttribute("y", String(y + h / 2 + 4));
    text.setAttribute("text-anchor", "middle");
    text.setAttribute("fill", textColor);
    text.setAttribute("font-weight", "bold");
    text.textContent = item.label;
    svg.appendChild(text);
  }

  // Tooltip element
  const tooltip = document.createElement("div");
  tooltip.style.cssText =
    "display:none;position:absolute;background:#252526;color:#d4d4d4;" +
    "border:1px solid #3c3c3c;border-radius:4px;padding:6px 10px;" +
    "font:12px 'SF Mono','Fira Code',Menlo,monospace;pointer-events:none;" +
    "white-space:pre;z-index:10;";
  container.appendChild(tooltip);

  // Build partner lookup: for each node key, find the partner key via edges
  const partnerMap = new Map<string, string>();
  for (const edge of tree.edges) {
    const k1 = `${edge.node1[0]},${edge.node1[1]}`;
    const k2 = `${edge.node2[0]},${edge.node2[1]}`;
    partnerMap.set(k1, k2);
    partnerMap.set(k2, k1);
  }

  // Hover: highlight partner + edge
  for (const [key, g] of nodeGroups) {
    const n = tree.nodes.find(
      (nd) => `${nd.premiseIndex},${nd.termIndex}` === key,
    )!;

    g.addEventListener("mouseenter", () => {
      // Highlight this node
      const r = g.querySelector("rect")!;
      r.setAttribute("stroke", "#569cd6");
      r.setAttribute("stroke-width", "2");
      // Highlight partner
      const pk = partnerMap.get(key);
      if (pk) {
        const pg = nodeGroups.get(pk);
        if (pg) {
          pg.querySelector("rect")!.setAttribute("stroke", "#569cd6");
          pg.querySelector("rect")!.setAttribute("stroke-width", "2");
        }
      }
      // Highlight edge
      if (n.pairIndex >= 0) {
        const el = edgeEls.get(n.pairIndex);
        if (el) {
          el.setAttribute("stroke-width", "3");
          el.setAttribute("opacity", "1");
        }
      }
    });

    g.addEventListener("mouseleave", () => {
      // Reset this node
      const r = g.querySelector("rect")!;
      r.setAttribute("stroke", "#3c3c3c");
      r.setAttribute("stroke-width", "1");
      // Reset partner
      const pk = partnerMap.get(key);
      if (pk) {
        const pg = nodeGroups.get(pk);
        if (pg) {
          pg.querySelector("rect")!.setAttribute("stroke", "#3c3c3c");
          pg.querySelector("rect")!.setAttribute("stroke-width", "1");
        }
      }
      // Reset edge
      if (n.pairIndex >= 0) {
        const el = edgeEls.get(n.pairIndex);
        if (el) {
          el.setAttribute("stroke-width", "1.5");
          el.setAttribute("opacity", "0.7");
        }
      }
      tooltip.style.display = "none";
    });

    // Click: show tooltip with details
    g.addEventListener("click", (ev) => {
      const st = n.signedTerm;
      const sign =
        st.sign.tag === "Wild" ? "*" : st.sign.sign === Sign.Plus ? "+" : "−";
      const comp =
        st.termExpr.tag === "Atomic" && st.termExpr.term.complemented
          ? " (complemented)"
          : "";
      const pos =
        st.positions.length > 0
          ? `\nPositions: <${st.positions.join(",")}>`
          : "";
      const termLabel =
        st.termExpr.tag === "Atomic" ? st.termExpr.term.termName : "(compound)";
      const status = n.cancelled ? "Cancelled" : "Uncancelled";
      tooltip.textContent =
        `${sign}${termLabel}${comp}${pos}\n` +
        `Premise ${n.premiseIndex + 1}, term ${n.termIndex + 1}\n` +
        `Status: ${status}`;
      tooltip.style.display = "block";
      const rect = svg.getBoundingClientRect();
      const cx = ev.clientX - rect.left;
      const cy = ev.clientY - rect.top;
      tooltip.style.left = cx + 8 + "px";
      tooltip.style.top = cy + 8 + "px";
    });
  }

  // Dismiss tooltip on background click
  svg.addEventListener("click", (ev) => {
    if (ev.target === bg) tooltip.style.display = "none";
  });

  container.appendChild(svg);

  // Export buttons
  const btnRow = document.createElement("div");
  btnRow.style.cssText = "display:flex;gap:6px;margin-top:4px;";
  const mkBtn = (label: string, handler: () => void) => {
    const btn = document.createElement("button");
    btn.textContent = label;
    btn.style.cssText =
      "background:#3c3c3c;color:#ccc;border:1px solid #555;" +
      "padding:2px 8px;border-radius:3px;font-size:11px;cursor:pointer;";
    btn.addEventListener("click", handler);
    return btn;
  };
  btnRow.appendChild(
    mkBtn("Export SVG", () => {
      const svgStr = renderTreeSvg(tree);
      downloadBlob(svgStr, "tree.svg", "image/svg+xml");
    }),
  );
  btnRow.appendChild(
    mkBtn("Export PNG", () => {
      exportPng(svg, svgW, svgH);
    }),
  );
  container.appendChild(btnRow);

  return container;
}

function downloadBlob(content: string, filename: string, mime: string): void {
  const blob = new Blob([content], { type: mime });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  a.click();
  URL.revokeObjectURL(url);
}

function exportPng(svgEl: SVGSVGElement, width: number, height: number): void {
  const serializer = new XMLSerializer();
  const svgStr = serializer.serializeToString(svgEl);
  const blob = new Blob([svgStr], { type: "image/svg+xml;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const img = new Image();
  img.onload = () => {
    const scale = 2; // retina
    const canvas = document.createElement("canvas");
    canvas.width = width * scale;
    canvas.height = height * scale;
    const ctx = canvas.getContext("2d")!;
    ctx.scale(scale, scale);
    ctx.drawImage(img, 0, 0);
    URL.revokeObjectURL(url);
    canvas.toBlob((pngBlob) => {
      if (!pngBlob) return;
      const pngUrl = URL.createObjectURL(pngBlob);
      const a = document.createElement("a");
      a.href = pngUrl;
      a.download = "tree.png";
      a.click();
      URL.revokeObjectURL(pngUrl);
    }, "image/png");
  };
  img.src = url;
}

// ---------------------------------------------------------------------------
// Shared SVG building helpers (used by renderTreeSvg)
// ---------------------------------------------------------------------------

function emitEdges(
  parts: string[],
  tree: TreeData,
  nodeRect: Map<string, Rect>,
): void {
  for (const edge of tree.edges) {
    const r1 = nodeRect.get(`${edge.node1[0]},${edge.node1[1]}`);
    const r2 = nodeRect.get(`${edge.node2[0]},${edge.node2[1]}`);
    if (r1 && r2) {
      parts.push(
        `<line x1="${r1.x + r1.w / 2}" y1="${r1.y + r1.h}" ` +
          `x2="${r2.x + r2.w / 2}" y2="${r2.y}" ` +
          `stroke="#f44747" stroke-width="1.5" stroke-dasharray="4,3" opacity="0.7"/>`,
      );
    }
  }
}

function emitPremiseNodes(parts: string[], premiseRows: LayoutItem[][]): void {
  for (const row of premiseRows) {
    for (const item of row) {
      const { x, y, w, h } = item.rect;
      const fill = item.node.cancelled ? "#2d2d2d" : "#264f78";
      const textColor = item.node.cancelled ? "#666" : "#4ec9b0";
      parts.push(
        `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="4" fill="${fill}" stroke="#3c3c3c"/>`,
      );
      parts.push(
        `<text x="${x + w / 2}" y="${y + h / 2 + 4}" text-anchor="middle" fill="${textColor}">${escSvg(item.label)}</text>`,
      );
      if (item.node.cancelled) {
        parts.push(
          `<line x1="${x + 4}" y1="${y + h / 2}" x2="${x + w - 4}" y2="${y + h / 2}" ` +
            `stroke="#f44747" stroke-width="1.5" opacity="0.6"/>`,
        );
      }
    }
  }
}

function emitFlowLines(
  parts: string[],
  tree: TreeData,
  nodeRect: Map<string, Rect>,
  conclItems: ConclItem[],
): void {
  for (const un of tree.uncancelledNodes) {
    const r = nodeRect.get(`${un.premiseIndex},${un.termIndex}`);
    if (r) {
      const ci = conclItems.findIndex(
        (c) => c.label === prettySignedTerm(un.signedTerm),
      );
      if (ci >= 0) {
        const cr = conclItems[ci].rect;
        parts.push(
          `<line x1="${r.x + r.w / 2}" y1="${r.y + r.h}" ` +
            `x2="${cr.x + cr.w / 2}" y2="${cr.y}" ` +
            `stroke="#4ec9b0" stroke-width="1.5" opacity="0.5"/>`,
        );
      }
    }
  }
}

function emitThereforeLine(
  parts: string[],
  svgW: number,
  conclusionY: number,
): void {
  const lineY = conclusionY - CONCL_GAP / 2;
  parts.push(
    `<line x1="${PAD}" y1="${lineY}" x2="${svgW - PAD}" y2="${lineY}" ` +
      `stroke="#569cd6" stroke-width="1" stroke-dasharray="6,4"/>`,
  );
  parts.push(
    `<text x="${PAD}" y="${lineY - 4}" fill="#569cd6" font-size="11">∴</text>`,
  );
}

function emitConclusionNodes(
  parts: string[],
  conclItems: ConclItem[],
  valid: boolean,
): void {
  for (const item of conclItems) {
    const { x, y, w, h } = item.rect;
    const fill = valid ? "#1b4332" : "#4a1919";
    const textColor = valid ? "#4ec9b0" : "#f44747";
    parts.push(
      `<rect x="${x}" y="${y}" width="${w}" height="${h}" rx="4" fill="${fill}" stroke="#3c3c3c"/>`,
    );
    parts.push(
      `<text x="${x + w / 2}" y="${y + h / 2 + 4}" text-anchor="middle" fill="${textColor}" font-weight="bold">${escSvg(item.label)}</text>`,
    );
  }
}

function escSvg(s: string): string {
  return s
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}
