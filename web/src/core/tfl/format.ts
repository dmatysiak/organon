// TFL Document formatter — ported from Organon.Syl.Lsp.Format (adapted for TFL)

/**
 * Format a .tfl document: normalize whitespace, canonicalize conclusion
 * markers, collapse blank lines.
 */
export function formatText(text: string): string {
  let lines = text.split("\n");
  lines = lines.map(stripEnd);
  lines = lines.map(canonicalizeTherefore);
  lines = normalizeTflLines(lines);
  lines = collapseBlankLines(lines);
  return lines.join("\n");
}

function stripEnd(line: string): string {
  return line.replace(/\s+$/, "");
}

/** Replace a leading "therefore" with "∴" on conclusion lines. */
function canonicalizeTherefore(line: string): string {
  const match = line.match(/^(\s*)therefore\s/i);
  if (match) {
    return match[1] + "∴ " + line.slice(match[0].length);
  }
  return line;
}

/** Normalize indentation and internal whitespace on TFL statement lines. */
function normalizeTflLines(lines: string[]): string[] {
  return lines.map((line) => {
    if (isTflStatementLine(line)) {
      const collapsed = line.trim().replace(/\s+/g, " ");
      return "  " + collapsed;
    }
    return line;
  });
}

function isTflStatementLine(line: string): boolean {
  const stripped = line.replace(/^\s+/, "");
  if (stripped.length === 0) return false;
  // Must have leading whitespace (indented under a proof block)
  if (stripped.length === line.length) return false;
  // TFL statements start with +, -, *, ∴, or English quantifiers
  return (
    stripped.startsWith("+") ||
    stripped.startsWith("-") ||
    stripped.startsWith("*") ||
    stripped.startsWith("∴") ||
    stripped.startsWith("?") ||
    stripped.startsWith("@") ||
    stripped.toLowerCase().startsWith("every ") ||
    stripped.toLowerCase().startsWith("no ") ||
    stripped.toLowerCase().startsWith("some ")
  );
}

/** Collapse runs of 3+ blank lines into 2. */
function collapseBlankLines(lines: string[]): string[] {
  const result: string[] = [];
  let blanks = 0;
  for (const line of lines) {
    if (line.trim() === "") {
      blanks++;
      if (blanks <= 2) result.push(line);
    } else {
      blanks = 0;
      result.push(line);
    }
  }
  return result;
}
