# organon-web

Browser-based proof assistant for medieval syllogistic logic. This is a
client-side-only web app — no server required. All parsing, validation
and proof reduction runs directly in the browser.

## Prerequisites

- Node.js 18+
- npm

## Setup

```
cd web
npm install
```

## Development

```
npm run dev
```

Opens a local dev server (default `http://localhost:5173/`) with hot reload.

## Build

```
npm run build
```

Produces a static site in `dist/`. The output is self-contained HTML, JS
and CSS — deploy it to any static file host.

## Preview production build

```
npm run preview
```

Serves the `dist/` directory locally to verify the production build.
Opening `dist/index.html` directly as a `file://` URL will not work —
browsers block ES modules and web workers over `file://`.

## Deploy

Copy the contents of `dist/` to any static hosting provider:

- GitHub Pages
- Netlify (drop folder)
- Vercel (`vercel --prod`)
- S3 + CloudFront
- Any web server serving static files

No environment variables or backend services needed.

## Usage

Type `.syl` proofs directly in the editor. The syntax is the same as
described in the main project README. Use the example buttons in the
toolbar to load sample proofs.

A minimal proof looks like:

```
proof Barbara
  every M is P
  every S is M
  ∴ every S is P
```

Add `tradition Strict`, `tradition Traditional` or `tradition Full` at
the top to select between 15, 19 or 24 valid moods (defaults to Full).

Use `?` as a placeholder for unknown terms, quantifiers or entire
propositions. The editor will offer code actions to fill them in.

## Features

- **Diagnostics** — invalid syllogisms, unknown references and parse
  errors appear as red/yellow squiggly underlines.
- **Hover** — hover over a proof name to see its mood, figure and
  full reduction proof. Hover over `@references` to see the resolved
  proposition.
- **Go to definition** — Ctrl-click (Cmd-click on macOS) an
  `@reference` to jump to the referenced proof.
- **Code actions** — click the lightbulb or press Cmd+. (Ctrl+.) to:
  - Swap premises into canonical order when they are reversed.
  - Fill holes (`?`) with all valid solutions, labeled by mood.
- **Syntax highlighting** — keywords, comments, references and holes
  are highlighted.
- **Example presets** — toolbar buttons load sample proofs (Basics,
  Holes).

## Project structure

```
web/
  index.html            Entry point
  vite.config.ts        Vite configuration
  tsconfig.json         TypeScript configuration
  src/
    main.ts             Monaco editor setup and UI wiring
    env.d.ts            Vite-specific type declarations
    core/               Ported logic (from Haskell src/Organon/Syl/)
      types.ts          Core types, enums, figure detection
      tradition.ts      Mood specs, valid mood lists per tradition
      proposition.ts    Conversions, obversion, contradictory
      validity.ts       Syllogism validation
      proof.ts          Reduction proofs to Figure I
      hole.ts           Constraint solver for holes
      pretty.ts         Human-readable formatting
      parser.ts         Recursive descent parser for .syl syntax
      document.ts       Document-level parser (proof blocks, directives)
      check.ts          Document checker (diagnostics, hovers, actions)
```

## Architecture

The app uses Monaco Editor (the editor component from VS Code) with a
custom Monarch tokenizer for `.syl` syntax highlighting. On every
keystroke the full document is re-parsed and re-checked, producing
diagnostics (error squiggles), hover information and code actions that
are pushed to the Monaco API.

The `core/` modules are a direct port of the Haskell library in
`src/Organon/Syl/`. All functions are pure — no IO, no network calls.
