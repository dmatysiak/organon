// Tradition mood specs — ported from Organon.Syl.Tradition

import { Figure, Mood, PropType, Tradition, ALL_MOODS } from "./types";

export type MoodSpec = {
  readonly moodFigure: Figure;
  readonly majorPropType: PropType;
  readonly minorPropType: PropType;
  readonly conclusionPropType: PropType;
};

const { A, E, I, O } = PropType;
const { FigI, FigII, FigIII, FigIV } = Figure;

const MOOD_SPECS: Record<Mood, MoodSpec> = {
  // Figure I
  [Mood.Barbara]: {
    moodFigure: FigI,
    majorPropType: A,
    minorPropType: A,
    conclusionPropType: A,
  },
  [Mood.Celarent]: {
    moodFigure: FigI,
    majorPropType: E,
    minorPropType: A,
    conclusionPropType: E,
  },
  [Mood.Darii]: {
    moodFigure: FigI,
    majorPropType: A,
    minorPropType: I,
    conclusionPropType: I,
  },
  [Mood.Ferio]: {
    moodFigure: FigI,
    majorPropType: E,
    minorPropType: I,
    conclusionPropType: O,
  },
  // Figure II
  [Mood.Cesare]: {
    moodFigure: FigII,
    majorPropType: E,
    minorPropType: A,
    conclusionPropType: E,
  },
  [Mood.Camestres]: {
    moodFigure: FigII,
    majorPropType: A,
    minorPropType: E,
    conclusionPropType: E,
  },
  [Mood.Festino]: {
    moodFigure: FigII,
    majorPropType: E,
    minorPropType: I,
    conclusionPropType: O,
  },
  [Mood.Baroco]: {
    moodFigure: FigII,
    majorPropType: A,
    minorPropType: O,
    conclusionPropType: O,
  },
  // Figure III
  [Mood.Darapti]: {
    moodFigure: FigIII,
    majorPropType: A,
    minorPropType: A,
    conclusionPropType: I,
  },
  [Mood.Disamis]: {
    moodFigure: FigIII,
    majorPropType: I,
    minorPropType: A,
    conclusionPropType: I,
  },
  [Mood.Datisi]: {
    moodFigure: FigIII,
    majorPropType: A,
    minorPropType: I,
    conclusionPropType: I,
  },
  [Mood.Felapton]: {
    moodFigure: FigIII,
    majorPropType: E,
    minorPropType: A,
    conclusionPropType: O,
  },
  [Mood.Bocardo]: {
    moodFigure: FigIII,
    majorPropType: O,
    minorPropType: A,
    conclusionPropType: O,
  },
  [Mood.Ferison]: {
    moodFigure: FigIII,
    majorPropType: E,
    minorPropType: I,
    conclusionPropType: O,
  },
  // Figure IV
  [Mood.Bramantip]: {
    moodFigure: FigIV,
    majorPropType: A,
    minorPropType: A,
    conclusionPropType: I,
  },
  [Mood.Camenes]: {
    moodFigure: FigIV,
    majorPropType: A,
    minorPropType: E,
    conclusionPropType: E,
  },
  [Mood.Dimaris]: {
    moodFigure: FigIV,
    majorPropType: I,
    minorPropType: A,
    conclusionPropType: I,
  },
  [Mood.Fesapo]: {
    moodFigure: FigIV,
    majorPropType: E,
    minorPropType: A,
    conclusionPropType: O,
  },
  [Mood.Fresison]: {
    moodFigure: FigIV,
    majorPropType: E,
    minorPropType: I,
    conclusionPropType: O,
  },
  // Subaltern
  [Mood.Barbari]: {
    moodFigure: FigI,
    majorPropType: A,
    minorPropType: A,
    conclusionPropType: I,
  },
  [Mood.Celaront]: {
    moodFigure: FigI,
    majorPropType: E,
    minorPropType: A,
    conclusionPropType: O,
  },
  [Mood.Cesaro]: {
    moodFigure: FigII,
    majorPropType: E,
    minorPropType: A,
    conclusionPropType: O,
  },
  [Mood.Camestrop]: {
    moodFigure: FigII,
    majorPropType: A,
    minorPropType: E,
    conclusionPropType: O,
  },
  [Mood.Calemos]: {
    moodFigure: FigIV,
    majorPropType: A,
    minorPropType: E,
    conclusionPropType: O,
  },
};

export function moodSpec(mood: Mood): MoodSpec {
  return MOOD_SPECS[mood];
}

const EXISTENTIAL_IMPORT: ReadonlySet<Mood> = new Set([
  Mood.Darapti,
  Mood.Felapton,
  Mood.Bramantip,
  Mood.Fesapo,
]);

export function requiresExistentialImport(mood: Mood): boolean {
  return EXISTENTIAL_IMPORT.has(mood);
}

const SUBALTERN: ReadonlySet<Mood> = new Set([
  Mood.Barbari,
  Mood.Celaront,
  Mood.Cesaro,
  Mood.Camestrop,
  Mood.Calemos,
]);

export function isSubaltern(mood: Mood): boolean {
  return SUBALTERN.has(mood);
}

export function validMoods(tradition: Tradition): Mood[] {
  return ALL_MOODS.filter((m) => {
    switch (tradition) {
      case Tradition.Strict:
        return !requiresExistentialImport(m) && !isSubaltern(m);
      case Tradition.Traditional:
        return !isSubaltern(m);
      case Tradition.Full:
        return true;
    }
  });
}
