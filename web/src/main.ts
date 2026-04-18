import * as monaco from "monaco-editor";
import editorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";

import { parseDocument } from "./core/document";
import { ParseError } from "./core/parser";
import {
  checkDocument,
  Severity,
  type CheckResult,
  type ExternalContext,
} from "./core/check";
import { prettyMood } from "./core/pretty";
import { formatText } from "./core/format";
import { initRepl } from "./repl";

// Monaco requires web workers for editor functionality.
self.MonacoEnvironment = {
  getWorker: () => new editorWorker(),
};

// -- Language registration ---------------------------------------------------

const LANG_ID = "syl";

monaco.languages.register({ id: LANG_ID, extensions: [".syl"] });

// Monarch tokenizer derived from the tmLanguage grammar.
monaco.languages.setMonarchTokensProvider(LANG_ID, {
  tokenizer: {
    root: [
      [/--.*$/, "comment"],
      [
        /^(tradition)(\s+)(Strict|Traditional|Full)\b/,
        ["keyword", "", "constant"],
      ],
      [/^(open)(\s+)(\S+)/, ["keyword", "", "type.identifier"]],
      [/^(proof)(\s+)(\S+)/, ["keyword", "", "function"]],
      [/∴|therefore\b/, "keyword"],
      [
        /(@[A-Za-z_][A-Za-z0-9_-]*\.[A-Za-z_][A-Za-z0-9_-]*)(\s+(?:conv|per-accidens)\b)?/,
        ["variable", "keyword"],
      ],
      [
        /(@[A-Za-z_][A-Za-z0-9_-]*)(\s+(?:conv|per-accidens)\b)?/,
        ["variable", "keyword"],
      ],
      [/\b(every|no|some)\b/i, "keyword"],
      [/\b(is\s+not|is)\b/, "keyword"],
      [/\?/, "variable"],
    ],
  },
});

// -- Unicode input abbreviations (Lean/Agda style) ---------------------------

const UNICODE_ABBREVS: Record<string, string> = {
  "\\tf": "∴",
  "\\therefore": "∴",
};

// -- Example content ---------------------------------------------------------

const EXAMPLES: Record<string, string> = {
  basics: `-- Fundamental syllogisms in all four figures.

tradition Full

proof Barbara
  every M is P
  every S is M
  ∴ every S is P

proof Celarent
  no M is P
  every S is M
  ∴ no S is P

proof Darii
  every M is P
  some S is M
  ∴ some S is P

proof Ferio
  no M is P
  some S is M
  ∴ some S is not P
`,
  holes: `-- Hole-based solving: use ? for unknowns.

tradition Full

proof FindConclusion
  every M is P
  every S is M
  ∴ ?

proof FindSubject
  every M is P
  every ? is M
  ∴ every ? is P
`,
};

// -- Tab / buffer management -------------------------------------------------

type Tab = {
  id: string;
  name: string;
  model: monaco.editor.ITextModel;
  fileHandle: FileSystemFileHandle | null;
  dirty: boolean;
  viewState: monaco.editor.ICodeEditorViewState | null;
};

let tabs: Tab[] = [];
let activeTabId: string | null = null;
let tabCounter = 0;

const tabBar = document.getElementById("tab-bar")!;

function createTab(
  name: string,
  content: string,
  fileHandle: FileSystemFileHandle | null = null,
): Tab {
  tabCounter++;
  const id = `tab-${tabCounter}`;
  const uri = monaco.Uri.parse(`inmemory://model/${id}/${name}`);
  const model = monaco.editor.createModel(content, LANG_ID, uri);
  const tab: Tab = {
    id,
    name,
    model,
    fileHandle,
    dirty: false,
    viewState: null,
  };

  model.onDidChangeContent(() => {
    tab.dirty = true;
    renderTabs();
    scheduleCheck();
  });

  tabs.push(tab);
  renderTabs();
  switchToTab(id);
  return tab;
}

function switchToTab(id: string): void {
  // Save current view state
  const current = tabs.find((t) => t.id === activeTabId);
  if (current) {
    current.viewState = editor.saveViewState();
  }

  activeTabId = id;
  const tab = tabs.find((t) => t.id === id);
  if (tab) {
    editor.setModel(tab.model);
    if (tab.viewState) {
      editor.restoreViewState(tab.viewState);
    }
    editor.focus();
    runCheck();
  }
  renderTabs();
}

function closeTab(id: string): void {
  const idx = tabs.findIndex((t) => t.id === id);
  if (idx === -1) return;
  const tab = tabs[idx];

  if (
    tab.dirty &&
    !confirm(`"${tab.name}" has unsaved changes. Close anyway?`)
  ) {
    return;
  }

  tab.model.dispose();
  tabs.splice(idx, 1);

  if (tabs.length === 0) {
    createTab("Untitled.syl", "");
  } else if (activeTabId === id) {
    const newIdx = Math.min(idx, tabs.length - 1);
    switchToTab(tabs[newIdx].id);
  } else {
    renderTabs();
  }
}

function renderTabs(): void {
  tabBar.innerHTML = "";
  for (const tab of tabs) {
    const el = document.createElement("div");
    el.className =
      "tab" +
      (tab.id === activeTabId ? " active" : "") +
      (tab.dirty ? " dirty" : "");

    const label = document.createElement("span");
    label.textContent = tab.name;
    label.addEventListener("click", () => switchToTab(tab.id));
    label.addEventListener("dblclick", (e) => {
      e.stopPropagation();
      startRenameTab(tab, label);
    });

    const dot = document.createElement("span");
    dot.className = "modified";

    const close = document.createElement("span");
    close.className = "close";
    close.textContent = "×";
    close.addEventListener("click", (e) => {
      e.stopPropagation();
      closeTab(tab.id);
    });

    el.append(dot, label, close);
    tabBar.appendChild(el);
  }
}

function startRenameTab(tab: Tab, label: HTMLElement): void {
  const input = document.createElement("input");
  input.type = "text";
  input.value = tab.name;
  input.style.cssText =
    "background:#1e1e1e;color:#fff;border:1px solid #007acc;font-size:12px;" +
    "padding:0 4px;outline:none;width:" +
    Math.max(60, label.offsetWidth) +
    "px;";

  const commit = () => {
    const raw = input.value.trim();
    if (raw.length > 0) {
      tab.name = raw.endsWith(".syl") ? raw : raw + ".syl";
    }
    renderTabs();
    editor.focus();
  };

  input.addEventListener("blur", commit);
  input.addEventListener("keydown", (e) => {
    if (e.key === "Enter") {
      e.preventDefault();
      input.blur();
    } else if (e.key === "Escape") {
      input.removeEventListener("blur", commit);
      renderTabs();
      editor.focus();
    }
  });

  label.replaceWith(input);
  input.select();
}

function activeTab(): Tab | null {
  return tabs.find((t) => t.id === activeTabId) ?? null;
}

// -- Editor setup ------------------------------------------------------------

const container = document.getElementById("editor-container")!;

const editor = monaco.editor.create(container, {
  language: LANG_ID,
  theme: "vs-dark",
  minimap: { enabled: false },
  fontSize: 15,
  lineNumbers: "on",
  renderWhitespace: "none",
  scrollBeyondLastLine: false,
  automaticLayout: true,
});

// Must be declared before createTab → switchToTab → runCheck uses it.
const emptyExt: ExternalContext = new Map();
let lastCheckResult: CheckResult | null = null;

// Create initial tab
createTab("Basics.syl", EXAMPLES.basics);

// -- Example buttons ---------------------------------------------------------

document
  .querySelectorAll<HTMLButtonElement>(".examples button")
  .forEach((btn) => {
    btn.addEventListener("click", () => {
      const key = btn.dataset.example;
      if (key && EXAMPLES[key]) {
        const name = key.charAt(0).toUpperCase() + key.slice(1) + ".syl";
        createTab(name, EXAMPLES[key]);
      }
    });
  });

// -- File operations ---------------------------------------------------------

document.getElementById("btn-new")!.addEventListener("click", () => {
  createTab("Untitled.syl", "");
});

document.getElementById("btn-open")!.addEventListener("click", async () => {
  if ("showOpenFilePicker" in window) {
    try {
      const [handle] = await (window as any).showOpenFilePicker({
        types: [
          {
            description: "Syllogistic files",
            accept: { "text/plain": [".syl"] },
          },
        ],
        multiple: false,
      });
      const file = await handle.getFile();
      const text = await file.text();
      createTab(file.name, text, handle);
    } catch {
      // User cancelled
    }
  } else {
    // Fallback: file input
    const input = document.createElement("input");
    input.type = "file";
    input.accept = ".syl";
    input.addEventListener("change", async () => {
      const file = input.files?.[0];
      if (file) {
        const text = await file.text();
        createTab(file.name, text);
      }
    });
    input.click();
  }
});

document
  .getElementById("btn-save")!
  .addEventListener("click", () => saveActiveTab());

async function saveActiveTab(): Promise<void> {
  const tab = activeTab();
  if (!tab) return;

  // Format on save
  const formatted = formatText(tab.model.getValue());
  if (formatted !== tab.model.getValue()) {
    tab.model.setValue(formatted);
  }

  if ("showSaveFilePicker" in window) {
    try {
      const handle =
        tab.fileHandle ??
        (await (window as any).showSaveFilePicker({
          suggestedName: tab.name,
          types: [
            {
              description: "Syllogistic files",
              accept: { "text/plain": [".syl"] },
            },
          ],
        }));
      const writable = await handle.createWritable();
      await writable.write(tab.model.getValue());
      await writable.close();
      tab.fileHandle = handle;
      tab.name = handle.name;
      tab.dirty = false;
      renderTabs();
    } catch {
      // User cancelled
    }
  } else {
    // Fallback: download
    const blob = new Blob([tab.model.getValue()], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = tab.name;
    a.click();
    URL.revokeObjectURL(url);
    tab.dirty = false;
    renderTabs();
  }
}

// Cmd/Ctrl+S to save
editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, () => {
  saveActiveTab();
});

// -- Unicode abbreviation expansion ------------------------------------------

editor.onDidChangeModelContent((e) => {
  const model = editor.getModel();
  if (!model) return;

  for (const change of e.changes) {
    // Check if the recently typed text completes an abbreviation
    const line = model.getLineContent(change.range.startLineNumber);
    const col = change.range.startColumn + change.text.length;
    const textBefore = line.slice(0, col - 1);

    for (const [abbrev, replacement] of Object.entries(UNICODE_ABBREVS)) {
      if (textBefore.endsWith(abbrev)) {
        const startCol = col - abbrev.length;
        const range = new monaco.Range(
          change.range.startLineNumber,
          startCol,
          change.range.startLineNumber,
          col,
        );
        // Use setTimeout to avoid modifying the model during a change event
        setTimeout(() => {
          model.pushEditOperations(
            [],
            [{ range, text: replacement }],
            () => null,
          );
        }, 0);
        return;
      }
    }
  }
});

// -- Diagnostics & checking --------------------------------------------------

function runCheck(): void {
  const model = editor.getModel();
  if (!model) return;

  const text = model.getValue();
  const parsed = parseDocument(text);

  if (parsed instanceof ParseError) {
    monaco.editor.setModelMarkers(model, LANG_ID, [
      {
        startLineNumber: parsed.line,
        startColumn: parsed.col,
        endLineNumber: parsed.line,
        endColumn: parsed.col + 1,
        message: parsed.message,
        severity: monaco.MarkerSeverity.Error,
      },
    ]);
    lastCheckResult = null;
    return;
  }

  const result = checkDocument(emptyExt, parsed);
  lastCheckResult = result;

  const markers: monaco.editor.IMarkerData[] = result.checkDiagnostics.map(
    (d) => ({
      startLineNumber: d.diagStart.posLine,
      startColumn: d.diagStart.posCol,
      endLineNumber: d.diagEnd.posLine,
      endColumn: d.diagEnd.posCol,
      message: d.diagMessage,
      severity:
        d.diagSeverity === Severity.Error
          ? monaco.MarkerSeverity.Error
          : monaco.MarkerSeverity.Warning,
    }),
  );

  monaco.editor.setModelMarkers(model, LANG_ID, markers);
}

let checkTimer: ReturnType<typeof setTimeout> | null = null;

function scheduleCheck(): void {
  if (checkTimer !== null) clearTimeout(checkTimer);
  checkTimer = setTimeout(runCheck, 150);
}

// Initial check
runCheck();

// -- Hover provider ----------------------------------------------------------

monaco.languages.registerHoverProvider(LANG_ID, {
  provideHover(_model, position) {
    if (!lastCheckResult) return null;

    for (const h of lastCheckResult.checkHovers) {
      if (
        position.lineNumber >= h.hoverStart.posLine &&
        position.lineNumber <= h.hoverEnd.posLine &&
        (position.lineNumber > h.hoverStart.posLine ||
          position.column >= h.hoverStart.posCol) &&
        (position.lineNumber < h.hoverEnd.posLine ||
          position.column <= h.hoverEnd.posCol)
      ) {
        return {
          range: new monaco.Range(
            h.hoverStart.posLine,
            h.hoverStart.posCol,
            h.hoverEnd.posLine,
            h.hoverEnd.posCol,
          ),
          contents: [
            {
              value: "```\n" + h.hoverText + "\n```",
            },
          ],
        };
      }
    }
    return null;
  },
});

// -- Code action provider (swap premises + hole fills) -----------------------

monaco.languages.registerCodeActionProvider(LANG_ID, {
  provideCodeActions(model, range) {
    if (!lastCheckResult) return { actions: [], dispose() {} };

    const actions: monaco.languages.CodeAction[] = [];

    // Swap actions
    for (const swap of lastCheckResult.checkSwaps) {
      if (!rangeOverlaps(range, swap.swapPrem1Start, swap.swapPrem2End))
        continue;

      const prem1Range = new monaco.Range(
        swap.swapPrem1Start.posLine,
        swap.swapPrem1Start.posCol,
        swap.swapPrem1End.posLine,
        swap.swapPrem1End.posCol,
      );
      const prem2Range = new monaco.Range(
        swap.swapPrem2Start.posLine,
        swap.swapPrem2Start.posCol,
        swap.swapPrem2End.posLine,
        swap.swapPrem2End.posCol,
      );

      const prem1Text = model.getValueInRange(prem1Range);
      const prem2Text = model.getValueInRange(prem2Range);

      actions.push({
        title: "Swap premises to canonical order",
        kind: "quickfix",
        edit: {
          edits: [
            {
              resource: model.uri,
              textEdit: { range: prem2Range, text: prem1Text },
              versionId: model.getVersionId(),
            },
            {
              resource: model.uri,
              textEdit: { range: prem1Range, text: prem2Text },
              versionId: model.getVersionId(),
            },
          ],
        },
        isPreferred: true,
      });
    }

    // Hole fills
    for (const fill of lastCheckResult.checkHoleFills) {
      if (fill.holeFillEdits.length === 0) continue;

      const overlaps = fill.holeFillEdits.some((e) =>
        rangeOverlaps(range, e.fillEditStart, e.fillEditEnd),
      );
      if (!overlaps) continue;

      const edits: monaco.languages.IWorkspaceTextEdit[] =
        fill.holeFillEdits.map((e) => ({
          resource: model.uri,
          textEdit: {
            range: new monaco.Range(
              e.fillEditStart.posLine,
              e.fillEditStart.posCol,
              e.fillEditEnd.posLine,
              e.fillEditEnd.posCol,
            ),
            text: e.fillEditText,
          },
          versionId: model.getVersionId(),
        }));

      actions.push({
        title: `${prettyMood(fill.holeFillMood)}: ${fill.holeFillLabel}`,
        kind: "quickfix",
        edit: { edits },
      });
    }

    // Reduce to Figure 1
    for (const ra of lastCheckResult.checkReduces) {
      if (!rangeOverlaps(range, ra.reducePrem1Start, ra.reduceConcEnd))
        continue;

      actions.push({
        title: `Reduce ${prettyMood(ra.reduceMood)} to Figure 1`,
        kind: "refactor.rewrite",
        edit: {
          edits: [
            {
              resource: model.uri,
              textEdit: {
                range: new monaco.Range(
                  ra.reducePrem1Start.posLine,
                  ra.reducePrem1Start.posCol,
                  ra.reducePrem1End.posLine,
                  ra.reducePrem1End.posCol,
                ),
                text: ra.reducePrem1Text,
              },
              versionId: model.getVersionId(),
            },
            {
              resource: model.uri,
              textEdit: {
                range: new monaco.Range(
                  ra.reducePrem2Start.posLine,
                  ra.reducePrem2Start.posCol,
                  ra.reducePrem2End.posLine,
                  ra.reducePrem2End.posCol,
                ),
                text: ra.reducePrem2Text,
              },
              versionId: model.getVersionId(),
            },
            {
              resource: model.uri,
              textEdit: {
                range: new monaco.Range(
                  ra.reduceConcStart.posLine,
                  ra.reduceConcStart.posCol,
                  ra.reduceConcEnd.posLine,
                  ra.reduceConcEnd.posCol,
                ),
                text: ra.reduceConcText,
              },
              versionId: model.getVersionId(),
            },
          ],
        },
      });
    }

    return { actions, dispose() {} };
  },
});

// -- Go to definition --------------------------------------------------------

monaco.languages.registerDefinitionProvider(LANG_ID, {
  provideDefinition(_model, position) {
    if (!lastCheckResult) return null;

    for (const d of lastCheckResult.checkDefinitions) {
      if (
        position.lineNumber >= d.defRefStart.posLine &&
        position.lineNumber <= d.defRefEnd.posLine &&
        (position.lineNumber > d.defRefStart.posLine ||
          position.column >= d.defRefStart.posCol) &&
        (position.lineNumber < d.defRefEnd.posLine ||
          position.column <= d.defRefEnd.posCol)
      ) {
        if (d.defTargetFile !== null) continue;

        return {
          uri: _model.uri,
          range: new monaco.Range(
            d.defTargetStart.posLine,
            d.defTargetStart.posCol,
            d.defTargetEnd.posLine,
            d.defTargetEnd.posCol,
          ),
        };
      }
    }
    return null;
  },
});

// -- Helpers -----------------------------------------------------------------

function rangeOverlaps(
  range: monaco.Range,
  start: { posLine: number; posCol: number },
  end: { posLine: number; posCol: number },
): boolean {
  const spanRange = new monaco.Range(
    start.posLine,
    start.posCol,
    end.posLine,
    end.posCol,
  );
  return monaco.Range.areIntersectingOrTouching(range, spanRange);
}

// -- REPL panel --------------------------------------------------------------

initRepl({
  outputEl: document.getElementById("repl-output")!,
  inputEl: document.getElementById("repl-input") as HTMLInputElement,
});

// -- Draggable splitter ------------------------------------------------------

{
  const splitter = document.getElementById("splitter")!;
  const editorPane = document.querySelector(".editor-pane") as HTMLElement;
  const replPanel = document.getElementById("repl-panel")!;

  let dragging = false;

  splitter.addEventListener("mousedown", (e) => {
    e.preventDefault();
    dragging = true;
    splitter.classList.add("dragging");
    document.body.style.cursor = "col-resize";
    document.body.style.userSelect = "none";
  });

  window.addEventListener("mousemove", (e) => {
    if (!dragging) return;
    const parent = editorPane.parentElement!;
    const rect = parent.getBoundingClientRect();
    const offsetX = e.clientX - rect.left;
    const splitterWidth = splitter.offsetWidth;
    const minEditor = 200;
    const minRepl = 200;
    const available = rect.width - splitterWidth;
    const editorW = Math.max(minEditor, Math.min(available - minRepl, offsetX));
    const replW = available - editorW;
    editorPane.style.flex = "none";
    editorPane.style.width = editorW + "px";
    replPanel.style.width = replW + "px";
  });

  window.addEventListener("mouseup", () => {
    if (!dragging) return;
    dragging = false;
    splitter.classList.remove("dragging");
    document.body.style.cursor = "";
    document.body.style.userSelect = "";
  });
}
