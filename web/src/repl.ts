// Interactive REPL — ported from Organon.Syl.Repl

import { Mood, Syllogism, Tradition } from "./core/types";
import {
  moodSpec,
  requiresExistentialImport,
  isSubaltern,
  validMoods,
} from "./core/tradition";
import { validate } from "./core/validity";
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

// -- State -------------------------------------------------------------------

let tradition: Tradition = Tradition.Traditional;

// -- Output rendering -------------------------------------------------------

type OutputLine = { text: string; cls: "info" | "error" | "prompt" | "result" };

export type ReplUI = {
  outputEl: HTMLElement;
  inputEl: HTMLInputElement;
};

function appendLines(ui: ReplUI, lines: OutputLine[]): void {
  for (const line of lines) {
    const div = document.createElement("div");
    div.className = `repl-line repl-${line.cls}`;
    div.textContent = line.text;
    ui.outputEl.appendChild(div);
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
  const res = validate(tradition, parsed);
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
    ...prettySyllogism(fig1).split("\n").map(result),
  ];
}

function handleProve(input: string): OutputLine[] {
  const parsed = parseSyllogism(input);
  if (parsed instanceof ParseError) {
    return [err(`Parse error: ${parsed.message}`)];
  }
  const res = validate(tradition, parsed);
  switch (res.tag) {
    case "Invalid":
      return [err(`Invalid: ${res.message}`)];
    case "Valid": {
      const steps = reduce(res.mood, parsed);
      return [
        ...prettyProof(res.mood, steps).split("\n").map(result),
        ...reducedLines(res.mood, parsed),
      ];
    }
    case "ValidSwapped": {
      const steps = reduce(res.mood, res.syllogism);
      return [
        info("(premises swapped)"),
        ...prettyProof(res.mood, steps).split("\n").map(result),
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
    lines.push(info(`  ${fig}: ${group.map(prettyMood).join(", ")}`));
  }
  return lines;
}

// -- Command dispatch --------------------------------------------------------

function dispatch(raw: string, ui: ReplUI): void {
  const input = raw.trim();
  if (input === "") return;

  // Show the prompt line
  appendLines(ui, [{ text: `organon-syl> ${input}`, cls: "prompt" }]);

  const spaceIdx = input.indexOf(" ");
  const cmd = (
    spaceIdx === -1 ? input : input.slice(0, spaceIdx)
  ).toLowerCase();
  const args = spaceIdx === -1 ? "" : input.slice(spaceIdx + 1).trim();

  let lines: OutputLine[];

  switch (cmd) {
    case ":help":
    case ":h":
      lines = handleHelp();
      break;
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
    case ":clear":
      ui.outputEl.innerHTML = "";
      return;
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

  appendLines(ui, lines);
}

// -- History -----------------------------------------------------------------

let history: string[] = [];
let historyIdx = -1;

// -- Init --------------------------------------------------------------------

export function initRepl(ui: ReplUI): void {
  // Welcome message
  appendLines(ui, [
    info("organon-syl — a proof assistant for syllogistic logic"),
    info("Type :help for available commands."),
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
