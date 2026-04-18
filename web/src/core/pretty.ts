// Pretty printing — ported from Organon.Syl.Pretty

import { PropositionH, Solution, SolutionProp, TermH } from "./hole";
import { moodSpec } from "./tradition";
import {
  Figure,
  Mood,
  ProofStep,
  PropType,
  Proposition,
  Syllogism,
  Term,
  Tradition,
} from "./types";

export function prettyTerm(t: Term): string {
  return t.complemented ? `non-${t.termName}` : t.termName;
}

export function prettyPropType(pt: PropType): string {
  return pt;
}

export function prettyProposition(p: Proposition): string {
  const s = prettyTerm(p.subject);
  const pr = prettyTerm(p.predicate);
  switch (p.propType) {
    case PropType.A:
      return `every ${s} is ${pr}`;
    case PropType.E:
      return `no ${s} is ${pr}`;
    case PropType.I:
      return `some ${s} is ${pr}`;
    case PropType.O:
      return `some ${s} is not ${pr}`;
  }
}

export function prettyFigure(fig: Figure): string {
  switch (fig) {
    case Figure.FigI:
      return "1";
    case Figure.FigII:
      return "2";
    case Figure.FigIII:
      return "3";
    case Figure.FigIV:
      return "4";
  }
}

export function prettyMood(mood: Mood): string {
  return mood;
}

export function prettyTradition(t: Tradition): string {
  switch (t) {
    case Tradition.Strict:
      return "Strict (15 moods)";
    case Tradition.Traditional:
      return "Traditional (19 moods)";
    case Tradition.Full:
      return "Full (24 moods)";
  }
}

export function prettySyllogism(syl: Syllogism): string {
  return [
    prettyProposition(syl.major),
    prettyProposition(syl.minor),
    `∴ ${prettyProposition(syl.conclusion)}`,
  ].join("\n");
}

export function prettyProofStep(step: ProofStep): string {
  switch (step.tag) {
    case "Axiom":
      return `Axiom: ${prettyMood(step.mood)} (perfect syllogism)`;
    case "SimpleConversion":
      return `Simple conversion: ${prettyProposition(step.from)} → ${prettyProposition(step.to)}`;
    case "ConversionPerAccidens":
      return `Conversion per accidens: ${prettyProposition(step.from)} → ${prettyProposition(step.to)}`;
    case "MutatePremises":
      return "Mutate: swap major and minor premises";
    case "ReductioAdImpossibile":
      return `Reductio ad impossibile: assume ${prettyProposition(step.assumed)}, derive contradiction via ${prettyMood(step.targetMood)}`;
    case "Subalternation":
      return `Subalternation: ${prettyProposition(step.from)} → ${prettyProposition(step.to)}`;
  }
}

export function prettyProof(mood: Mood, steps: ProofStep[]): string {
  const header = `Reduction of ${prettyMood(mood)}:`;
  const lines = steps.map((step, i) => `  ${i + 1}. ${prettyProofStep(step)}`);
  return [header, ...lines].join("\n");
}

export function prettyTermH(th: TermH): string {
  return th.tag === "ConcreteT" ? prettyTerm(th.term) : "?";
}

export function prettyPropositionH(ph: PropositionH): string {
  if (ph.tag === "WholePropH") return "?";
  const s = prettyTermH(ph.subject);
  const p = prettyTermH(ph.predicate);
  if (ph.propTypeH.tag === "HolePT") return `? ${s} is ${p}`;
  switch (ph.propTypeH.propType) {
    case PropType.A:
      return `every ${s} is ${p}`;
    case PropType.E:
      return `no ${s} is ${p}`;
    case PropType.I:
      return `some ${s} is ${p}`;
    case PropType.O:
      return `some ${s} is not ${p}`;
  }
}

function prettyMaybeTerm(t: Term | null, label: string): string {
  return t !== null ? prettyTerm(t) : label;
}

export function prettySolutionProp(
  sp: SolutionProp,
  labels: [string, string],
): string {
  const s = prettyMaybeTerm(sp.solSubject, labels[0]);
  const p = prettyMaybeTerm(sp.solPredicate, labels[1]);
  switch (sp.solPropType) {
    case PropType.A:
      return `every ${s} is ${p}`;
    case PropType.E:
      return `no ${s} is ${p}`;
    case PropType.I:
      return `some ${s} is ${p}`;
    case PropType.O:
      return `some ${s} is not ${p}`;
  }
}

function figureLabels(
  fig: Figure,
): [[string, string], [string, string], [string, string]] {
  switch (fig) {
    case Figure.FigI:
      return [
        ["?M", "?P"],
        ["?S", "?M"],
        ["?S", "?P"],
      ];
    case Figure.FigII:
      return [
        ["?P", "?M"],
        ["?S", "?M"],
        ["?S", "?P"],
      ];
    case Figure.FigIII:
      return [
        ["?M", "?P"],
        ["?M", "?S"],
        ["?S", "?P"],
      ];
    case Figure.FigIV:
      return [
        ["?P", "?M"],
        ["?M", "?S"],
        ["?S", "?P"],
      ];
  }
}

export function prettySolution(sol: Solution): string {
  const spec = moodSpec(sol.solutionMood);
  const triple = `${prettyPropType(spec.majorPropType)}${prettyPropType(spec.minorPropType)}${prettyPropType(spec.conclusionPropType)}`;
  const fig = prettyFigure(spec.moodFigure);
  const [majLabels, minLabels, conLabels] = figureLabels(spec.moodFigure);
  return (
    `${prettyMood(sol.solutionMood)} (${triple}-${fig}): ` +
    `${prettySolutionProp(sol.solMajor, majLabels)}; ` +
    `${prettySolutionProp(sol.solMinor, minLabels)}; ` +
    `∴ ${prettySolutionProp(sol.solConclusion, conLabels)}`
  );
}
