// Interactive REPL — supports both Syl and TFL

import { Mood, Syllogism, Tradition } from "./core/types";
import {
  moodSpec,
  requiresExistentialImport,
  isSubaltern,
  validMoods,
} from "./core/tradition";
import { validate as sylValidate } from "./core/validity";
import { reduce, reducedSyllogism } from "./core/proof";
import { solve } from "./core/hole";
import { parseSyllogism, parseSyllogismH, ParseError } from "./core/parser";
import {
  prettyMood,
  prettyTradition,
  prettyPropType,
  prettyFigure,
  prettyProof,
  prettySolution,
  prettySyllogism,
} from "./core/pretty";

// TFL imports
import {
  parseInference as parseTflInference,
  ParseError as TflParseError,
} from "./core/tfl/parser";
import { validate as tflValidate } from "./core/tfl/validity";
import type { Inference as TflInference } from "./core/tfl/types";
import type { ValidationResult as TflValidationResult } from "./core/tfl/validity";
import {
  prettyInference as prettyTflInference,
  prettyCancellation,
  prettyValidationResult,
} from "./core/tfl/pretty";
import { buildTree, renderTreeInteractive } from "./core/tfl/tree";

type Lang = "syl" | "tfl";

// -- State -------------------------------------------------------------------

let tradition: Tradition = Tradition.Traditional;
let lang: Lang = "syl";
let lastTflInference: TflInference | null = null;
let lastTflResult: TflValidationResult | null = null;

// -- Output rendering -------------------------------------------------------

type OutputLine =
  | { text: string; cls: "info" | "error" | "prompt" | "result" }
  | { html: string; cls: "svg" }
  | { element: HTMLElement; cls: "dom" };

export type ReplUI = {
  outputEl: HTMLElement;
  inputEl: HTMLInputElement;
};

function appendLines(ui: ReplUI, lines: OutputLine[]): void {
  for (const line of lines) {
    if ("element" in line) {
      const div = document.createElement("div");
      div.className = "repl-line repl-svg";
      div.appendChild(line.element);
      ui.outputEl.appendChild(div);
    } else if ("html" in line) {
      const div = document.createElement("div");
      div.className = "repl-line repl-svg";
      div.innerHTML = line.html;
      ui.outputEl.appendChild(div);
    } else {
      const div = document.createElement("div");
      div.className = `repl-line repl-${line.cls}`;
      div.textContent = line.text;
      ui.outputEl.appendChild(div);
    }
  }
  ui.outputEl.scrollTop = ui.outputEl.scrollHeight;
}

function info(text: string): OutputLine {
  return { text, cls: "info" };
}

function err(text: string): OutputLine {
  return { text, cls: "error" };
}

function result(text: string): OutputLine {
  return { text, cls: "result" };
}

// -- Command handlers --------------------------------------------------------

function handleHelp(): OutputLine[] {
  if (lang === "tfl") {
    return [
      info("Commands:"),
      info(
        "  :validate <inference>   Check cancellation validity (default for bare input)",
      ),
      info(
        "  :tree [<inference>]     Show cancellation tree (last validated if no arg)",
      ),
      info("  :lang syl|tfl           Switch language"),
      info("  :clear                  Clear output"),
      info("  :help                   Show this help"),
      info(""),
      info("Inference format (semicolons separate premises from conclusion):"),
      info("  −S +M; −M +P; −S +P"),
      info("  every S is M; every M is P; every S is P"),
      info(""),
      info("Shortcuts: :v, :tr, :l, :h"),
    ];
  }
  return [
    info("Commands:"),
    info("  :validate <syllogism>   Check validity (default for bare input)"),
    info("  :prove <syllogism>      Validate and show reduction proof"),
    info(
      "  :solve <pattern>        Find valid syllogisms matching pattern (use ? for holes)",
    ),
    info("  :tradition <name>       Set tradition: strict, traditional, full"),
    info("  :moods                  List valid moods for current tradition"),
    info("  :mood <name>            Show details of a mood"),
    info("  :clear                  Clear output"),
    info("  :help                   Show this help"),
    info(""),
    info("Syllogism format:"),
    info("  Every M is P; Every S is M; Every S is P"),
    info("  No P is M; Some S is M; therefore Some S is not P"),
    info(""),
    info("Holes (use ? for unknowns):"),
    info("  ?                       Unknown proposition"),
    info("  ? S is P                Unknown quantifier"),
    info("  Every ? is P            Unknown subject term"),
    info(""),
    info("Shortcuts: :v, :p, :s, :t, :h"),
  ];
}

function handleTradition(args: string): OutputLine[] {
  switch (args.toLowerCase()) {
    case "strict":
      tradition = Tradition.Strict;
      return [info(`Tradition set to ${prettyTradition(tradition)}`)];
    case "traditional":
      tradition = Tradition.Traditional;
      return [info(`Tradition set to ${prettyTradition(tradition)}`)];
    case "full":
      tradition = Tradition.Full;
      return [info(`Tradition set to ${prettyTradition(tradition)}`)];
    default:
      return [err("Usage: :tradition strict|traditional|full")];
  }
}

function validLine(mood: Mood, suffix: string): string {
  const spec = moodSpec(mood);
  const triple =
    prettyPropType(spec.majorPropType) +
    prettyPropType(spec.minorPropType) +
    prettyPropType(spec.conclusionPropType);
  const fig = prettyFigure(spec.moodFigure);
  return `Valid: ${prettyMood(mood)} (${triple}-${fig})${suffix}`;
}

function handleValidate(input: string): OutputLine[] {
  const parsed = parseSyllogism(input);
  if (parsed instanceof ParseError) {
    return [err(`Parse error: ${parsed.message}`)];
  }
  const res = sylValidate(tradition, parsed);
  switch (res.tag) {
    case "Valid":
      return [result(validLine(res.mood, ""))];
    case "ValidSwapped":
      return [result(validLine(res.mood, " (premises swapped)"))];
    case "Invalid":
      return [err(`Invalid: ${res.message}`)];
  }
}

function reducedLines(mood: Mood, syl: Syllogism): OutputLine[] {
  const fig1 = reducedSyllogism(mood, syl);
  if (fig1 === null) return [];
  return [
    info("Figure 1 form:"),
    ...prettySyllogism(fig1)
      .split("\n")
      .map((l) => result(`  ${l}`)),
  ];
}

function handleProve(input: string): OutputLine[] {
  const parsed = parseSyllogism(input);
  if (parsed instanceof ParseError) {
    return [err(`Parse error: ${parsed.message}`)];
  }
  const res = sylValidate(tradition, parsed);
  switch (res.tag) {
    case "Invalid":
      return [err(`Invalid: ${res.message}`)];
    case "Valid": {
      const steps = reduce(res.mood, parsed);
      const [proofHeader, ...proofSteps] = prettyProof(res.mood, steps).split(
        "\n",
      );
      return [
        info(proofHeader),
        ...proofSteps.map(result),
        ...reducedLines(res.mood, parsed),
      ];
    }
    case "ValidSwapped": {
      const steps = reduce(res.mood, res.syllogism);
      const [proofHeader, ...proofSteps] = prettyProof(res.mood, steps).split(
        "\n",
      );
      return [
        info("(premises swapped)"),
        info(proofHeader),
        ...proofSteps.map(result),
        ...reducedLines(res.mood, res.syllogism),
      ];
    }
  }
}

function handleSolve(input: string): OutputLine[] {
  const parsed = parseSyllogismH(input);
  if (parsed instanceof ParseError) {
    return [err(`Parse error: ${parsed.message}`)];
  }
  const solutions = solve(tradition, parsed);
  if (solutions.length === 0) {
    return [info("No valid syllogisms match this pattern.")];
  }
  return solutions.map((sol) => result(prettySolution(sol)));
}

function lookupMood(name: string): Mood | null {
  const target = name.toLowerCase();
  const all = Object.values(Mood);
  const found = all.filter((m) => m.toLowerCase() === target);
  return found.length === 1 ? found[0] : null;
}

function handleMoodInfo(args: string): OutputLine[] {
  const mood = lookupMood(args);
  if (!mood) {
    return [err(`Unknown mood: ${args}`)];
  }
  const spec = moodSpec(mood);
  const triple =
    prettyPropType(spec.majorPropType) +
    prettyPropType(spec.minorPropType) +
    prettyPropType(spec.conclusionPropType);
  const fig = prettyFigure(spec.moodFigure);
  const flags: string[] = [];
  if (requiresExistentialImport(mood)) flags.push("existential import");
  if (isSubaltern(mood)) flags.push("subaltern");
  const flagStr = flags.length > 0 ? ` (${flags.join(", ")})` : "";
  const valid = validMoods(tradition).includes(mood);
  return [
    result(`${prettyMood(mood)}: ${triple}-${fig}${flagStr}`),
    info(
      `Valid in current tradition (${prettyTradition(tradition)}): ${valid ? "yes" : "no"}`,
    ),
  ];
}

function handleMoods(): OutputLine[] {
  const moods = validMoods(tradition);
  const lines: OutputLine[] = [
    info(`Valid moods under ${prettyTradition(tradition)}:`),
  ];

  // Group by figure
  const byFigure = new Map<string, Mood[]>();
  for (const m of moods) {
    const fig = prettyFigure(moodSpec(m).moodFigure);
    const group = byFigure.get(fig) ?? [];
    group.push(m);
    byFigure.set(fig, group);
  }

  for (const [fig, group] of byFigure) {
    lines.push(result(`  ${fig}: ${group.map(prettyMood).join(", ")}`));
  }
  return lines;
}

// -- TFL command handlers ----------------------------------------------------

function handleTflValidate(input: string): OutputLine[] {
  const parsed = parseTflInference(input);
  if (parsed instanceof TflParseError) {
    return [err(`Parse error: ${parsed.message}`)];
  }
  const res = tflValidate(parsed);
  lastTflInference = parsed;
  lastTflResult = res;
  return prettyValidationResult(res)
    .split("\n")
    .map((l) => (res.tag === "Valid" ? result(l) : err(l)));
}

function handleTree(args: string): OutputLine[] {
  let inf: TflInference | null = null;
  let res: TflValidationResult | null = null;

  if (args.trim().length > 0) {
    const parsed = parseTflInference(args);
    if (parsed instanceof TflParseError) {
      return [err(`Parse error: ${parsed.message}`)];
    }
    res = tflValidate(parsed);
    inf = parsed;
    lastTflInference = inf;
    lastTflResult = res;
  } else {
    inf = lastTflInference;
    res = lastTflResult;
  }

  if (!inf || !res) {
    return [
      err(
        "No inference to visualize. Run :validate first, or pass an inference.",
      ),
    ];
  }

  const tree = buildTree(inf, res);
  const el = renderTreeInteractive(tree);
  return [{ element: el, cls: "dom" as const }];
}

function handleLang(args: string, ui: ReplUI): OutputLine[] {
  switch (args.toLowerCase()) {
    case "syl":
      lang = "syl";
      updatePrompt(ui);
      return [info("Switched to syllogistic logic.")];
    case "tfl":
      lang = "tfl";
      updatePrompt(ui);
      return [info("Switched to term functor logic.")];
    default:
      return [err("Usage: :lang syl|tfl")];
  }
}

function updatePrompt(ui: ReplUI): void {
  const label = ui.inputEl.parentElement?.querySelector("label");
  if (label) {
    label.textContent = `organon/${lang}>`;
  }
}

// -- Command dispatch --------------------------------------------------------

function dispatch(raw: string, ui: ReplUI): void {
  const input = raw.trim();
  if (input === "") return;

  // Show the prompt line
  appendLines(ui, [{ text: `organon/${lang}> ${input}`, cls: "prompt" }]);

  const spaceIdx = input.indexOf(" ");
  const cmd = (
    spaceIdx === -1 ? input : input.slice(0, spaceIdx)
  ).toLowerCase();
  const args = spaceIdx === -1 ? "" : input.slice(spaceIdx + 1).trim();

  let lines: OutputLine[];

  // Language-independent commands
  switch (cmd) {
    case ":help":
    case ":h":
      lines = handleHelp();
      appendLines(ui, lines);
      return;
    case ":lang":
    case ":l":
      lines = handleLang(args, ui);
      appendLines(ui, lines);
      return;
    case ":clear":
      ui.outputEl.innerHTML = "";
      return;
  }

  if (lang === "tfl") {
    // TFL dispatch
    switch (cmd) {
      case ":validate":
      case ":v":
        lines = handleTflValidate(args);
        break;
      case ":tree":
      case ":tr":
        lines = handleTree(args);
        break;
      default:
        if (cmd.startsWith(":")) {
          lines = [
            err(`Unknown command: ${cmd}. Type :help for available commands.`),
          ];
        } else {
          lines = handleTflValidate(input);
        }
        break;
    }
  } else {
    // Syl dispatch
    switch (cmd) {
      case ":tradition":
      case ":t":
        lines = handleTradition(args);
        break;
      case ":validate":
      case ":v":
        lines = handleValidate(args);
        break;
      case ":prove":
      case ":p":
        lines = handleProve(args);
        break;
      case ":solve":
      case ":s":
        lines = handleSolve(args);
        break;
      case ":mood":
        lines = handleMoodInfo(args);
        break;
      case ":moods":
        lines = handleMoods();
        break;
      default:
        if (cmd.startsWith(":")) {
          lines = [
            err(`Unknown command: ${cmd}. Type :help for available commands.`),
          ];
        } else if (input.includes("?")) {
          lines = handleSolve(input);
        } else {
          lines = handleValidate(input);
        }
        break;
    }
  }

  appendLines(ui, lines);
}

// -- History -----------------------------------------------------------------

let history: string[] = [];
let historyIdx = -1;

// -- Init --------------------------------------------------------------------

export function initRepl(ui: ReplUI): void {
  // Welcome message
  appendLines(ui, [
    info("organon — type :help for commands, :lang syl|tfl to switch"),
    info(""),
  ]);

  ui.inputEl.addEventListener("keydown", (e) => {
    // Prevent Monaco from capturing keys when REPL input is focused
    e.stopPropagation();

    if (e.key === "Enter") {
      const value = ui.inputEl.value;
      if (value.trim() !== "") {
        history.push(value);
        historyIdx = history.length;
      }
      dispatch(value, ui);
      ui.inputEl.value = "";
    } else if (e.key === "ArrowUp") {
      e.preventDefault();
      if (historyIdx > 0) {
        historyIdx--;
        ui.inputEl.value = history[historyIdx];
      }
    } else if (e.key === "ArrowDown") {
      e.preventDefault();
      if (historyIdx < history.length - 1) {
        historyIdx++;
        ui.inputEl.value = history[historyIdx];
      } else {
        historyIdx = history.length;
        ui.inputEl.value = "";
      }
    }
  });
}
