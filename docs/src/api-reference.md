# CLI

`cl-cc` is the single public entry point. This page lists every command and
flag; for worked examples see [Recipes](recipes.md).

The argument parser is the external
[cl-cli](https://github.com/nerima-lisp/cl-cli) library. The `completion`,
`docs`, and `version` commands are generated from the same declarative
application spec that drives parsing, so `cl-cc docs markdown` always matches
the flags the binary actually accepts.

## Common flags

Every executing command accepts a timeout pair:

| Flag | Meaning |
|---|---|
| `--timeout <seconds>` | maximum execution time (default: `30`) |
| `--no-timeout` | disable the CLI timeout, for debugging |

Every command that reads a source file accepts `--lang` to override the
frontend that would otherwise be chosen from the file extension:

```text
--lang lisp|elisp|php|js|javascript
```

## Commands

### `cl-cc run`

```text
cl-cc run <file>
  --lang <language>       Source language (auto-detected from extension)
  --timeout <seconds>
  --no-timeout
```

Compiles and runs a `.lisp`, `.php`, `.js`, or `.mjs` file.

### `cl-cc compile`

```text
cl-cc compile <file>
  --lang <language>       Source language (auto-detected from extension)
  --arch x86-64|arm64
  -o <output>
  --timeout <seconds>
  --no-timeout
```

Compiles to a native Mach-O binary.

### `cl-cc eval`

```text
cl-cc eval "<expr>"
  --timeout <seconds>
  --no-timeout
```

Evaluates a single expression and prints the result.

### `cl-cc repl`

```text
cl-cc repl
  --stdlib                Include the higher-order function library
  --timeout <seconds>     Per-form execution timeout (default: 30)
  --no-timeout
```

Starts the interactive REPL. Definitions persist across forms within the
session.

### `cl-cc check`

```text
cl-cc check <file>
  --lang <language>       Source language (auto-detected from extension)
  --strict                Treat type warnings as errors
  --timeout <seconds>
  --no-timeout
```

Type-checks without executing. This runs Hindley–Milner inference
independently of the run pipeline, so it can be used as a fast gate.

### `cl-cc selfhost`

```text
cl-cc selfhost [file]
  --timeout <seconds>
  --no-timeout
```

Runs the self-hosting workload. See
[Architecture](architecture.md#self-hosting).

### `cl-cc dep-graph`

```text
cl-cc dep-graph
  --format dot|json|mermaid|topo    Output format (default: dot)
```

Renders the ASDF dependency graph, modelled with
[cl-dataflow-kit](https://github.com/nerima-lisp/cl-dataflow-kit).

### `cl-cc completion`

```text
cl-cc completion <shell>
```

Emits a shell completion script. Supported shells are `bash`, `zsh`, `fish`,
`powershell`, `nushell`, and `elvish`.

### `cl-cc docs`

```text
cl-cc docs [markdown|man|json]
```

Emits the CLI reference documentation in the requested format.

### `cl-cc version`

```text
cl-cc version
```

Prints the cl-cc version.

## Environment variables

These affect the test runner rather than the CLI itself; see
[Getting started](getting-started.md#test-timeouts).

| Variable | Scope | Default |
|---|---|---|
| `CLCC_TEST_TIMEOUT` | one test | `10` |
| `CLCC_SUITE_TIMEOUT` | the whole suite | `600` |

## External libraries at the boundary

Several sibling nerima-lisp libraries back the CLI surface:

| Library | Role |
|---|---|
| [cl-cli](https://github.com/nerima-lisp/cl-cli) | argument parsing, `completion` / `docs` / `version` |
| [cl-tty-kit](https://github.com/nerima-lisp/cl-tty-kit) | terminal styling for the REPL and IR dumps |
| [cl-boundary-kit](https://github.com/nerima-lisp/cl-boundary-kit) | process-exit I/O, so tests can capture exit codes |
| [cl-dataflow-kit](https://github.com/nerima-lisp/cl-dataflow-kit) | the `dep-graph` command |
| [cl-parser-kit](https://github.com/nerima-lisp/cl-parser-kit) | tokenizing the optimizer `--pass-pipeline` spec |

Terminal styling is gated on an interactive TTY, so captured or piped output
stays plain.
