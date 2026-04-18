// Proposition operations — ported from Organon.Syl.Proposition

import { PropType, Proposition, Term } from "./types";

export function negateTerm(t: Term): Term {
  return { termName: t.termName, complemented: !t.complemented };
}

export function simpleConversion(p: Proposition): Proposition | null {
  if (p.propType === PropType.E || p.propType === PropType.I) {
    return { propType: p.propType, subject: p.predicate, predicate: p.subject };
  }
  return null;
}

export function conversionPerAccidens(p: Proposition): Proposition | null {
  if (p.propType === PropType.A) {
    return { propType: PropType.I, subject: p.predicate, predicate: p.subject };
  }
  if (p.propType === PropType.E) {
    return { propType: PropType.O, subject: p.predicate, predicate: p.subject };
  }
  return null;
}

export function obversion(p: Proposition): Proposition {
  const negP = negateTerm(p.predicate);
  switch (p.propType) {
    case PropType.A:
      return { propType: PropType.E, subject: p.subject, predicate: negP };
    case PropType.E:
      return { propType: PropType.A, subject: p.subject, predicate: negP };
    case PropType.I:
      return { propType: PropType.O, subject: p.subject, predicate: negP };
    case PropType.O:
      return { propType: PropType.I, subject: p.subject, predicate: negP };
  }
}

export function contradictory(p: Proposition): Proposition {
  switch (p.propType) {
    case PropType.A:
      return {
        propType: PropType.O,
        subject: p.subject,
        predicate: p.predicate,
      };
    case PropType.E:
      return {
        propType: PropType.I,
        subject: p.subject,
        predicate: p.predicate,
      };
    case PropType.I:
      return {
        propType: PropType.E,
        subject: p.subject,
        predicate: p.predicate,
      };
    case PropType.O:
      return {
        propType: PropType.A,
        subject: p.subject,
        predicate: p.predicate,
      };
  }
}

export function contraposition(p: Proposition): Proposition | null {
  const obverted = obversion(p);
  const converted =
    simpleConversion(obverted) ?? conversionPerAccidens(obverted);
  return converted ? obversion(converted) : null;
}
