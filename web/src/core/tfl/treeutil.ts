// Shared utilities for tree SVG rendering (termtree, predtree, tree)

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

export const NODE_H = 28;
export const CHAR_W = 7.2;
export const NODE_PAD_X = 16;
export const FONT_FAMILY = "'SF Mono','Fira Code',Menlo,monospace";
export const FONT_SIZE = "12";
export const BG_COLOR = "#1e1e1e";
export const NODE_FILL = "#264f78";
export const NODE_TEXT_COLOR = "#d4d4d4";
export const LEGEND_COLOR = "#969696";

// ---------------------------------------------------------------------------
// Node sizing
// ---------------------------------------------------------------------------

/** Compute node width from label text. */
export function nodeWidth(label: string): number {
  return Math.max(40, label.length * CHAR_W + NODE_PAD_X);
}

// ---------------------------------------------------------------------------
// SVG helpers
// ---------------------------------------------------------------------------

const NS = "http://www.w3.org/2000/svg";

/** Create the root SVG element with standard font settings. */
export function createSvg(width: number, height: number): SVGSVGElement {
  const svg = document.createElementNS(NS, "svg");
  svg.setAttribute("width", String(width));
  svg.setAttribute("height", String(height));
  svg.setAttribute("font-family", FONT_FAMILY);
  svg.setAttribute("font-size", FONT_SIZE);
  return svg;
}

/** Append a full-size background rect. */
export function appendBackground(svg: SVGSVGElement, width: number, height: number): void {
  const bg = document.createElementNS(NS, "rect");
  bg.setAttribute("width", String(width));
  bg.setAttribute("height", String(height));
  bg.setAttribute("fill", BG_COLOR);
  bg.setAttribute("rx", "4");
  svg.appendChild(bg);
}

/** Draw a labeled node box (rect + centered text). Returns the <g> element. */
export function drawNode(
  svg: SVGSVGElement,
  x: number,
  y: number,
  w: number,
  label: string,
  opts?: {
    fill?: string;
    stroke?: string;
    strokeWidth?: string;
    tooltip?: string;
  },
): SVGGElement {
  const g = document.createElementNS(NS, "g");
  g.style.cursor = "default";

  const rect = document.createElementNS(NS, "rect");
  rect.setAttribute("x", String(x));
  rect.setAttribute("y", String(y));
  rect.setAttribute("width", String(w));
  rect.setAttribute("height", String(NODE_H));
  rect.setAttribute("rx", "4");
  rect.setAttribute("fill", opts?.fill ?? NODE_FILL);
  rect.setAttribute("stroke", opts?.stroke ?? "#3c3c3c");
  if (opts?.strokeWidth) rect.setAttribute("stroke-width", opts.strokeWidth);
  g.appendChild(rect);

  const text = document.createElementNS(NS, "text");
  text.setAttribute("x", String(x + w / 2));
  text.setAttribute("y", String(y + NODE_H / 2 + 4));
  text.setAttribute("text-anchor", "middle");
  text.setAttribute("fill", NODE_TEXT_COLOR);
  text.textContent = label;
  g.appendChild(text);

  if (opts?.tooltip) {
    const title = document.createElementNS(NS, "title");
    title.textContent = opts.tooltip;
    g.appendChild(title);
  }

  svg.appendChild(g);
  return g;
}

/** Draw a line between two points. */
export function drawLine(
  svg: SVGSVGElement,
  x1: number,
  y1: number,
  x2: number,
  y2: number,
  color: string,
  opts?: { dashed?: boolean; opacity?: number; strokeWidth?: number },
): SVGLineElement {
  const line = document.createElementNS(NS, "line");
  line.setAttribute("x1", String(x1));
  line.setAttribute("y1", String(y1));
  line.setAttribute("x2", String(x2));
  line.setAttribute("y2", String(y2));
  line.setAttribute("stroke", color);
  line.setAttribute("stroke-width", String(opts?.strokeWidth ?? 1.5));
  if (opts?.dashed) line.setAttribute("stroke-dasharray", "4,3");
  line.setAttribute("opacity", String(opts?.opacity ?? 0.8));
  svg.appendChild(line);
  return line;
}

/** Draw an arrow (line + arrowhead) between two points. */
export function drawArrow(
  svg: SVGSVGElement,
  x1: number,
  y1: number,
  x2: number,
  y2: number,
  color: string,
  opts?: { dashed?: boolean; opacity?: number },
): void {
  const op = opts?.opacity ?? 0.8;
  drawLine(svg, x1, y1, x2, y2, color, {
    dashed: opts?.dashed,
    opacity: op,
  });

  const angle = Math.atan2(y2 - y1, x2 - x1);
  const headLen = 8;
  const ax = x2 - headLen * Math.cos(angle - Math.PI / 6);
  const ay = y2 - headLen * Math.sin(angle - Math.PI / 6);
  const bx = x2 - headLen * Math.cos(angle + Math.PI / 6);
  const by = y2 - headLen * Math.sin(angle + Math.PI / 6);
  const poly = document.createElementNS(NS, "polygon");
  poly.setAttribute("points", `${x2},${y2} ${ax},${ay} ${bx},${by}`);
  poly.setAttribute("fill", color);
  poly.setAttribute("opacity", String(op));
  svg.appendChild(poly);
}

/** Draw an edge between two node centers, choosing top/bottom vs side connection. */
export function drawNodeEdge(
  svg: SVGSVGElement,
  n: { x: number; y: number; w: number },
  m: { x: number; y: number; w: number },
  color: string,
  opts?: { dashed?: boolean; opacity?: number; arrow?: boolean },
): void {
  let x1: number, y1: number, x2: number, y2: number;

  if (n.y === m.y) {
    // Same level: center-to-center
    x1 = n.x + n.w / 2;
    y1 = n.y + NODE_H / 2;
    x2 = m.x + m.w / 2;
    y2 = m.y + NODE_H / 2;
  } else {
    // Different levels: bottom of upper → top of lower
    const [upper, lower] = n.y < m.y ? [n, m] : [m, n];
    x1 = upper.x + upper.w / 2;
    y1 = upper.y + NODE_H;
    x2 = lower.x + lower.w / 2;
    y2 = lower.y;
  }

  if (opts?.arrow) {
    drawArrow(svg, x1, y1, x2, y2, color, opts);
  } else {
    drawLine(svg, x1, y1, x2, y2, color, opts);
  }
}

export type LegendItem = {
  color: string;
  label: string;
  dash: boolean;
};

/** Draw a horizontal legend row at the given y position. */
export function drawLegend(
  svg: SVGSVGElement,
  items: LegendItem[],
  x: number,
  y: number,
): void {
  let lx = x;
  for (const item of items) {
    const line = document.createElementNS(NS, "line");
    line.setAttribute("x1", String(lx));
    line.setAttribute("y1", String(y));
    line.setAttribute("x2", String(lx + 20));
    line.setAttribute("y2", String(y));
    line.setAttribute("stroke", item.color);
    line.setAttribute("stroke-width", "2");
    if (item.dash) line.setAttribute("stroke-dasharray", "4,3");
    svg.appendChild(line);
    const lbl = document.createElementNS(NS, "text");
    lbl.setAttribute("x", String(lx + 24));
    lbl.setAttribute("y", String(y + 4));
    lbl.setAttribute("fill", LEGEND_COLOR);
    lbl.setAttribute("font-size", "10");
    lbl.textContent = item.label;
    svg.appendChild(lbl);
    lx += 24 + item.label.length * 6 + 16;
  }
}
