# organon-syl

A proof assistant for medieval syllogistic logic — figures, moods and reductions.

## Building

```
stack build
```

## REPL

```
stack exec organon-syl-repl
```

The REPL supports validation, proof by reduction to Figure 1, and
hole-based solving. Type `:help` for commands.

```
organon-syl> every M is P; every S is M; every S is P
Valid: Barbara (Figure 1)

organon-syl> :prove every M is P; every S is M; every S is P
Reduction of Barbara:
  1. Axiom: Barbara (perfect syllogism)

organon-syl> :prove every P is M; no S is M; no S is P
Reduction of Camestres:
  1. Simple conversion: no S is M → no M is S
  2. Mutate: swap major and minor premises
  3. Axiom: Celarent (perfect syllogism)
  4. Simple conversion: no P is S → no S is P

organon-syl> :solve every M is P; every S is M; ?
2 solutions found:
  Barbara (Figure 1): every S is P
  Barbari (Figure 1): some S is P
```

Use `:tradition strict|traditional|full` to select between 15, 19 or 24
valid moods.

## LSP server

```
stack exec organon-syl-lsp
```

The server communicates over stdio and provides:

- **Diagnostics** — invalid syllogisms, unknown `@references`, parse errors
- **Hover** — mood, figure, reduction steps on proof names; resolved propositions on `@references`
- **Go to definition** — jump from `@references` to the referenced proof
- **Code actions** — auto-swap premises; fill holes with solved propositions
- **Completion** — `@` triggers completion of proof names

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
proof Step4
  @Basics.Darii
  @Barbara
  ∴ some S is P

-- qualified references work without open
proof Step5
  @Basics.Darii
  @Cesare
  ∴ some S is P
```

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

## Running tests

```
stack test
```

## VS Code extension

The extension in `editors/vscode/` provides syntax highlighting, snippets,
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
vsce package --allow-missing-repository -o organon-syl-0.1.0.vsix
```

This produces `organon-syl-0.1.0.vsix` in the extension directory.

### Installing the extension

First install:

```
code --install-extension editors/vscode/organon-syl-0.1.0.vsix
```

Reinstalling (after a rebuild):

```
code --uninstall-extension organon.organon-syl
code --install-extension editors/vscode/organon-syl-0.1.0.vsix
```

After installing or reinstalling, reload the VS Code window
(Cmd+Shift+P → "Developer: Reload Window").

### LSP server setup

Place the `organon-syl-lsp` binary on your `PATH`:

```
stack install
```

Or point the extension at a specific binary via VS Code settings:

```json
{
  "organon-syl.serverPath": "/path/to/.stack-work/install/.../bin/organon-syl-lsp"
}
```

Opening any `.syl` file starts the LSP server automatically.
