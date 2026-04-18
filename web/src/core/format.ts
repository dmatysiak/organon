// Document formatter — ported from Organon.Syl.Lsp.Format

/**
 * Format a .syl document: normalize whitespace, canonicalize conclusion
 * markers, collapse blank lines.
 */
export function formatText(text: string): string {
  let lines = text.split("\n");
  lines = lines.map(stripEnd);
  lines = lines.map(canonicalizeTherefore);
  lines = normalizePropositions(lines);
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

/** Normalize indentation and internal whitespace on proposition lines. */
function normalizePropositions(lines: string[]): string[] {
  return lines.map((line) => {
    if (isPropositionLine(line)) {
      const collapsed = line.trim().replace(/\s+/g, " ");
      return "  " + canonicalizeQuantifier(collapsed);
    }
    return line;
  });
}

function isPropositionLine(line: string): boolean {
  const stripped = line.replace(/^\s+/, "");
  if (stripped.length === 0) return false;
  // Must have leading whitespace (indented under a proof block)
  if (stripped.length === line.length) return false;
  const lower = stripped.toLowerCase();
  return (
    lower.startsWith("every ") ||
    lower.startsWith("no ") ||
    lower.startsWith("some ") ||
    stripped.startsWith("∴ ") ||
    stripped.startsWith("@")
  );
}

function canonicalizeQuantifier(text: string): string {
  if (text.startsWith("∴ ")) {
    return "∴ " + canonicalizeQuantifier(text.slice(2));
  }
  const lower = text.toLowerCase();
  if (lower.startsWith("every ")) return "every " + text.slice(6);
  if (lower.startsWith("no ")) return "no " + text.slice(3);
  if (lower.startsWith("some ")) return "some " + text.slice(5);
  return text;
}

/** Collapse consecutive blank lines into a single blank line. */
function collapseBlankLines(lines: string[]): string[] {
  const result: string[] = [];
  let prevBlank = false;
  for (const line of lines) {
    if (line.length === 0) {
      if (!prevBlank) result.push(line);
      prevBlank = true;
    } else {
      result.push(line);
      prevBlank = false;
    }
  }
  return result;
}
