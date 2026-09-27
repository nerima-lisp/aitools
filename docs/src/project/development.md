# Development

## Running the tests

Inside the development shell, the dependencies are already on
`CL_SOURCE_REGISTRY`:

```sh
nix develop
sbcl --script run-tests.lisp
```

Outside it, point `CL_SOURCE_REGISTRY` at a checkout of every dependency
first: cl-cli, cl-weave, cl-json-kit, cl-regex-kit, cl-codec-kit,
cl-host-kit, cl-boundary-kit, cl-concurrent-kit, cl-process-kit, cl-vcs-kit,
and their own dependencies cl-date-kit, cl-log-kit, and cl-parser-kit, at
the tags `flake.nix` pins:

```sh
export CL_SOURCE_REGISTRY="$HOME/src/nerima-lisp//:"
sbcl --script run-tests.lisp
```

`run-tests.lisp` registers the checkout itself with `(:directory root)` and
inherits the rest of the registry, then runs `asdf:test-system "aitools"`.
It exits 1 with a one-line message if the test operation signals an error,
and 0 otherwise. Exit 0 means only that nothing signalled; read cl-weave's
report for how many specs ran and passed. There is no per-context filter.

The same suite runs in Nix:

```sh
nix run .#test
```

## The gate

```sh
nix flake check --print-build-logs
```

This is what CI runs on `x86_64-linux` (`.github/workflows/ci.yml`). The
checks are:

| Check | What it runs |
|---|---|
| `checks.<system>.default` | The test suite against the pinned dependencies. `CL_PROCESS_KIT_SPAWN` is set so the `bg` specs run, and `AITOOLS_E2E_BINARY` points `t/e2e/` at the delivered package. |
| `checks.<system>.paredit-lint` | paredit-cli's structural lint over every Lisp source: delimiters must balance. It does not check layout. |
| `checks.<system>.formatting` | treefmt over the Nix sources. |
| `checks.<system>.docs` | `mkdocs build --strict` of this site. |

To include files git does not track yet, use a `path:` flake reference:
`nix flake check path:. --print-build-logs`.

In a linked git worktree, `checks.formatting` fails inside the Nix sandbox
because `.git` there is a file pointing outside the sandbox and treefmt
calls `git ls-files`. Run `nix fmt` from a normal checkout instead; do not
weaken the check.

To build one check or the binary alone:

```sh
system=$(nix eval --raw --impure --expr 'builtins.currentSystem')
nix build ".#checks.$system.paredit-lint" --print-build-logs
nix build ".#packages.$system.docs" --no-link --print-build-logs
nix build                    # -> ./result/bin/aitools
```

`packages.default` also carries `cl-process-kit-spawn`, the helper that
`bg start` uses to detach, next to `bin/aitools`.

## Building the binary

`nix build` produces the delivered binary in two SBCL processes
(`overrideOutputs` in `flake.nix`):

1. `aitools-cli-fasls` compiles and loads every fasl of the `aitools/cli`
   closure through cl-nix-forge's `lispDerivation`, without dumping an image.
2. cl-nix-forge's `mkExecutable` runs `asdf:program-op` on `aitools/cli`
   over that precompiled tree in a fresh process. The fasls carry the same
   normalized store timestamp as their sources, so ASDF loads them without
   recompiling, and the dumped core never shares a heap with the compiler.

The result is copied, with `cl-process-kit-spawn` added to `bin/`, into
`packages.default`. The pinned cl-nix-forge is v0.6.1, whose `.asd` reader
evaluates a large `.asd` such as `aitools.asd` within Nix's default
`max-call-depth`; see [Benchmarks](../reference/benchmarks.md#known-dependency-issues).

## Test layout

```text
t/unit/<context>/     domain and application, with ports built from fakes
t/integration/        the structure test, gitignore parity with git,
                      real-filesystem adapters, crash recovery, CLI round trips
t/perf/               allocation tests
t/support/            JSON assertions and the store fault-injection adapter
t/e2e/                the built binary against the shell commands it replaces
                      (AITOOLS_E2E_BINARY names the binary; otherwise the
                      harness builds one)
```

Tests use cl-weave (`describe`, `it`, `expect`); no other test framework is
used. `t/support/store-fault-injection.lisp` binds the store's fault hook so
that a named step of the write protocol can throw (an in-process model of a
crash that still releases the flock), signal an I/O error, or end a forked
child process outright. The recovery tests use it to stop a write after each
step and check the next run's `recovered` result.

## The component lists

`aitools.asd` lists every context's files inline, grouped by context. Each
context contributes four kinds of file:

- **library** files (`domain/`, `application/`, `infrastructure/`), relative
  to the context's `src/`, in load order. They go into the `aitools` system.
- **presentation** files, also under the context's `src/`; only `aitools/cli`
  loads them.
- **data** files, relative to the repository's `data/`. Every context's data
  files load before any library file, into the shared `aitools.data` package.
- **test** files, relative to `t/`, loaded into `aitools/test`.

The contexts appear in load order: core kernel, protocol, workspace, text,
store, then feature search, inspect, journal, edit, process, vcs, env, util.
A context loads after every context whose application package it calls:
journal before edit (edit registers its `tx rebase` replayers with journal),
inspect before edit and vcs, and edit before util.

To add a context or move files between layers, edit its lists in
`aitools.asd` directly.

## Adding a command

1. Put the logic in the owning context: pure rules in `domain/`, the flow in
   `application/` as a function that calls exactly one of its continuations.
2. In `presentation/`, build the cl-cli command and a
   `make-command-schema` value, and call
   `aitools.protocol.application:register-command` from
   `register-<context>-commands (registry ports)`:

    ```lisp
    (aitools.protocol.application:register-command
     registry
     :name "json.get"   ; dispatch name; "read" for an ungrouped command
     :group "json"      ; NIL for an ungrouped command
     :cli-command (cl-cli:make-command :name "get" ... :handler #'%handler)
     :schema (aitools.protocol.domain:make-command-schema "json.get" "Summary." ...))
    ```

    A grouped command's `:cli-command` carries the bare subcommand name; the
    composition root builds the group wrapper (`finalize-app-commands` in
    `src/registry.lisp`).
3. Wrap the flow in the handler with
   `aitools.protocol.application:call-with-command-result/k`.
4. If the context needs new effects, add them to its ports structure and to
   `make-production-<context>-ports` in `infrastructure/`.
5. Keep table data (patterns, summaries, option descriptions where the
   context keeps them as data) in `data/<layer>/<context>/*-data.lisp`.
6. Add specs under `t/`, list the new files in `aitools.asd`, and
   [regenerate the reference](#regenerating-the-reference).

`src/context-registration.lisp` finds `register-<context>-commands` and
`make-production-<context>-ports` by name for each feature context, so a new
command in an existing context needs no change in `src/`.

## Regenerating the reference

The command sections of [Commands](../reference/commands.md), the error
table in [Errors](../reference/errors.md), and the correspondence table in
[Using aitools from an agent](../guide/agents.md) are generated. Regenerate
them after changing a command schema, the error catalog, or
`data/domain/protocol/correspondence-table-data.lisp`:

```sh
sbcl --script docs/tools/generate-reference.lisp
```

It needs the same `CL_SOURCE_REGISTRY` as `run-tests.lisp`. It loads
`aitools/cli`, calls each context's `register-<context>-commands` into a
fresh registry, and rewrites only the text between each
`<!-- BEGIN GENERATED: name -->` and `<!-- END GENERATED: name -->` pair.
Review the diff before committing.
