# organon

A proof assistant for categorical logic — traditional syllogistic and
term functor logic (TFL).

## Building

```
stack build
```

## REPL

```
stack exec organon-repl
```

The REPL supports validation, proof by reduction to Figure 1 and
hole-based solving. Type `:help` for commands.

```
organon> every M is P; every S is M; every S is P
Valid: Barbara (AAA-1)

organon> :prove every M is P; every S is M; every S is P
Reduction of Barbara:
  1. Axiom: Barbara (perfect syllogism)

organon> :prove every P is M; no S is M; no S is P
Reduction of Camestres:
  1. Simple conversion: no S is M → no M is S
  2. Mutate: swap major and minor premises
  3. Axiom: Celarent (perfect syllogism)
  4. Simple conversion: no P is S → no S is P

organon> :solve every M is P; every S is M; ?
2 solutions found:
  Barbara (AAA-1): every S is P
  Barbari (AAI-1): some S is P
```

Use `:tradition strict|traditional|full` to select between 15, 19 or 24
valid moods.

## LSP server

```
stack exec organon-lsp
```

The server communicates over stdio and provides:

- **Diagnostics** — invalid syllogisms, unknown `@references`, parse errors
- **Hover** — mood, figure, triple notation (e.g. AAA-1), canonical form and reduction steps on proof names; resolved propositions on `@references` (with converted form when a modifier is applied)
- **Go to definition** — jump from `@references` to the referenced proof
- **Code actions** — auto-swap premises; fill holes with solved propositions; reduce to Figure 1
- **Completion** — `@` triggers completion of proof names
- **Formatting** — normalizes whitespace and casing on save or manual format

### `.syl` file format

```
-- optional tradition directive
tradition Full

-- import proofs from another file (namespace = filename stem)
open Basics

proof Barbara
  every M is P
  every S is M
  ∴ every S is P

proof Cesare
  no P is M
  every S is M
  ∴ no S is P

-- reference a proof in this file
proof Step3
  @Barbara
  @Cesare
  ∴ no S is P

-- reference a proof from an opened namespace
-- qualified references work without open
proof Step4
  @Basics.Darii
  @Barbara
  ∴ some S is P
```

### Reference modifiers

When chaining proofs, a referenced conclusion sometimes needs to be
converted before it can serve as a premise. Instead of restating the
proposition, append a modifier to the reference:

```
proof Step1
  no M is P
  every S is M
  ∴ no S is P

-- simple conversion: no S is P → no P is S
proof Step2
  @Step1 conv
  every M is P
  ∴ no M is S

-- conversion per accidens: every S is P → some P is S
proof Step3
  @Barbara per-accidens
  every P is M
  ∴ some M is S
```

This way, we preserve the provenance of propositions.

Available modifiers:

- `conv` — simple conversion (valid on E and I propositions)
- `per-accidens` — conversion per accidens (valid on A and E propositions)

The checker validates that the modifier is legal for the resolved
proposition type and reports an error otherwise.

The "Reduce to Figure 1" code action automatically emits `@ref conv` or
`@ref per-accidens` when a referenced premise needs conversion.

### Holes

Use `?` anywhere a term, quantifier or entire proposition can appear.
The LSP runs the solver and offers code actions to fill each hole.

```
-- whole conclusion unknown: solver finds valid conclusions
proof FindConclusion
  every M is P
  every S is M
  ∴ ?

-- term holes: unknown subject or predicate
proof FindSubject
  every M is P
  every ? is M
  ∴ every ? is P

-- quantifier hole: unknown proposition type
proof FindQuantifier
  every M is P
  every S is M
  ∴ ? S is P

-- whole premise unknown
proof FindPremise
  ?
  every S is M
  ∴ every S is P
```

### Namespaces

Each `.syl` file is a namespace. The namespace name is the filename stem
(case-sensitive): `Basics.syl` → `Basics`.

Use `open Basics` to bring all names from `Basics.syl` into scope.
Unqualified `@Name` refs resolve locally first, then in opened namespaces.
Qualified `@Ns.Name` refs work without `open`.

If an unqualified name exists in multiple opened namespaces, the LSP
reports an ambiguity error.

Each file validates its own proofs under its own `tradition` directive.
Cross-tradition references are allowed — a proved conclusion is a valid
premise regardless of which tradition validated it.

## Batch checker

```
stack exec organon-check -- [file.syl | dir ...]
```

Checks `.syl` files and prints diagnostics in `file:line:col: severity: message`
format. Defaults to the current directory. Silent on success; exits non-zero on errors.

```
$ stack exec organon-check -- examples/Basics.syl examples/God.syl
$ echo $?
0
```

## Term Functor Logic (TFL)

Organon includes a second language, TFL, based on Sommers' term functor
logic. Where syllogistic uses regimented English ("every S is P"), TFL
uses signed terms with algebraic cancellation to determine validity.

### Notation

Each term carries a sign: `+` (particular/affirmative) or `-` (universal/negative).

| English         | Algebraic |
| --------------- | --------- |
| every S is P    | `- S + P` |
| no S is P       | `- S - P` |
| some S is P     | `+ S + P` |
| some S is not P | `+ S - P` |

A wildcard sign `*` marks a term whose quantity is unspecified (used for
holes and complementation-indifferent positions).

### Validity by cancellation

An inference is valid when its middle terms cancel — each middle term
appears with opposite signs across premises — and the remaining
uncancelled terms match the conclusion.

```
- S + M   - M + P   ∴ - S + P
```

Here `+ M` and `- M` cancel, leaving `- S + P`.

### Sorites

TFL naturally handles multi-premise chains (sorites) that would require
separate syllogisms in traditional logic:

```
- A + B   - B + C   - C + D   ∴ - A + D
```

### Relational terms

Terms can carry positional subscripts for relational reasoning:

```
- Boy<1> + Love<1,2> + Girl<2>
```

This reads: every Boy is a Lover-of some Girl. The positions bind
terms into argument slots of the relation.

English rendering uses `-of` (active) and `-by` (passive) suffixes by
default. Custom forms are declared with `rel`:

```
rel Love "Lover-of" "Loved-by"
```

### Complementation

`non-` prefixes a complemented term:

```
- S + non-P
```

In English: "every S is non-P".

### `.tfl` file format

```
-- import conclusions from another .tfl file
open Basics

-- declare relational term forms (optional)
rel Love "Lover-of" "Loved-by"

proof Barbara
  - M + P
  - S + M
  ∴ - S + P

-- English syntax works too
proof BarbaraEng
  every M is P
  every S is M
  ∴ every S is P

-- reference a prior conclusion as a premise
proof Chain
  @Barbara
  - P + Q
  ∴ - S + Q

-- reference modifiers: conv, per-accidens, obv, contra
proof Converted
  @Barbara conv
  - P + Q
  ∴ - P + Q
```

### TFL reference modifiers

| Modifier       | Effect                                      | Valid on |
| -------------- | ------------------------------------------- | -------- |
| `conv`         | swap terms (simple conversion)              | E, I     |
| `per-accidens` | swap terms + weaken universal to particular | A, E     |
| `obv`          | flip one sign, complement its term          | all      |
| `contra`       | flip all signs, complement all terms        | A, O     |

### TFL holes

Use `?` for unknown terms, signs or entire statements:

```
proof FindConclusion
  - M + P
  - S + M
  ∴ ?

proof FindPremise
  ?
  - S + M
  ∴ - S + P
```

The LSP and REPL solve holes by computing valid cancellations.

### TFL REPL

```
stack exec organon-repl tfl
```

Commands:

| Command                     | Description                                  |
| --------------------------- | -------------------------------------------- |
| `- S + M; - M + P; - S + P` | validate an inference (default)              |
| `:validate <inf>`           | check cancellation validity                  |
| `:prove <inf>`              | validate and show which terms cancel         |
| `:solve <premises>`         | compute valid conclusion (use `?` for holes) |
| `:output tfl\|english`      | set display mode                             |
| `:help`                     | show available commands                      |

### TFL LSP features

The LSP server handles `.tfl` files with the same feature set as `.syl`:

- **Diagnostics** — invalid inferences, unknown `@references`, parse errors
- **Hover** — cancellation details and English rendering on proof names;
  resolved statements on `@references`
- **Go to definition** — jump from `@references` to the referenced proof
- **Code actions** — fill holes with solved conclusions
- **Completion** — `@` triggers completion of proof names
- **Formatting** — normalizes whitespace on save or manual format

## Running tests

```
stack test
```

## VS Code extension

The extension in `editors/vscode/` provides syntax highlighting, snippets
and an LSP client that auto-starts the server when you open a `.syl` file.

### Building the extension

```
cd editors/vscode
npm install
npm run compile
```

Use `npm run watch` during development for automatic recompilation.

### Packaging the VSIX

Requires [`vsce`](https://github.com/microsoft/vscode-vsce):

```
npm install -g @vscode/vsce   # one-time
cd editors/vscode
vsce package --allow-missing-repository -o organon-0.1.0.vsix
```

This produces `organon-0.1.0.vsix` in the extension directory.

### Installing the extension

First install:

```
code --install-extension editors/vscode/organon-0.1.0.vsix
```

Reinstalling (after a rebuild):

```
code --uninstall-extension organon.organon
code --install-extension editors/vscode/organon-0.1.0.vsix
```

After installing or reinstalling, reload the VS Code window
(Cmd+Shift+P → "Developer: Reload Window").

### LSP server setup

Place the `organon-lsp` binary on your `PATH`:

```
stack install
```

Or point the extension at a specific binary via VS Code settings:

```json
{
  "organon.serverPath": "/path/to/.stack-work/install/.../bin/organon-lsp"
}
```

Opening any `.syl` file starts the LSP server automatically.

## Demo

Try organon in your browser [here](https://dmatysiak.github.io/organon/).

The web UI provides:

- **Tabbed editor** with syntax highlighting, diagnostics and hover for `.syl` and `.tfl` files
- **New / Open / Save** buttons (double-click a tab to rename)
- **Example files** for both Syl and TFL
- **Built-in REPL** panel with the same commands as the terminal REPL
- **Code actions** — swap premises, fill holes, reduce to Figure 1 (Syl); fill holes (TFL)
- **Format on save** via Cmd/Ctrl+S
