// Syllogism validation — ported from Organon.Syl.Validity

import { Figure, Mood, PropType, Syllogism, Tradition, figure } from "./types";
import { moodSpec, validMoods } from "./tradition";

export type ValidationResult =
  | { readonly tag: "Valid"; readonly mood: Mood }
  | {
      readonly tag: "ValidSwapped";
      readonly mood: Mood;
      readonly syllogism: Syllogism;
    }
  | { readonly tag: "Invalid"; readonly message: string };

export function identifyMood(
  tradition: Tradition,
  syl: Syllogism,
): Mood | null {
  const fig = figure(syl);
  if (fig === null) return null;
  const pt: [PropType, PropType, PropType] = [
    syl.major.propType,
    syl.minor.propType,
    syl.conclusion.propType,
  ];
  return findMood(fig, pt, validMoods(tradition));
}

export function validate(
  tradition: Tradition,
  syl: Syllogism,
): ValidationResult {
  const m = identifyMood(tradition, syl);
  if (m !== null) return { tag: "Valid", mood: m };

  const swapped: Syllogism = {
    major: syl.minor,
    minor: syl.major,
    conclusion: syl.conclusion,
  };
  const ms = identifyMood(tradition, swapped);
  if (ms !== null) return { tag: "ValidSwapped", mood: ms, syllogism: swapped };

  const fig1 = figure(syl);
  const fig2 = figure(swapped);
  if (fig1 === null && fig2 === null) {
    return {
      tag: "Invalid",
      message:
        "Propositions do not form a well-structured syllogism (no consistent middle term)",
    };
  }
  return {
    tag: "Invalid",
    message: "Not a valid syllogism in the given tradition",
  };
}

function findMood(
  fig: Figure,
  pt: [PropType, PropType, PropType],
  moods: Mood[],
): Mood | null {
  for (const m of moods) {
    const spec = moodSpec(m);
    if (
      spec.moodFigure === fig &&
      spec.majorPropType === pt[0] &&
      spec.minorPropType === pt[1] &&
      spec.conclusionPropType === pt[2]
    ) {
      return m;
    }
  }
  return null;
}
