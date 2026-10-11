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
and 0 otherwise. The policy in `t/package.lisp` rejects failures, errors,
too few executed specs, and skips or todos outside its explicit allowlist.
Read cl-weave's report for the executed count and any allowed unrun specs.
There is no per-context filter.

For the full-suite host gate against the Nix-delivered binary, run:

```sh
nix run .#test
```

`apps.test` starts a fresh SBCL process and loads `run-host-tests.lisp` from
the flake's filtered source tree in the Nix store, not the writable checkout.
It builds `CL_SOURCE_REGISTRY` inline from that source and the pinned test
dependencies, and sets
`AITOOLS_E2E_BINARY` to the delivered `bin/aitools`. Each invocation
creates a fresh ASDF compilation cache in `AITOOLS_HOST_CACHE_DIR` and
ignores inherited output translations, so it cannot reuse the host's old
compiled files.

The runner loads `aitools/test` and runs every registered spec without a
context filter. Success requires a nonempty selection, every selected spec
passing, and zero skips, todos, failures, or errors, even for skips or todos
the ordinary test policy would allow. It also enforces
`*minimum-executed-specs*` in `t/package.lisp`. The report includes the store
source (`SOURCE`), delivered binary (`BINARY`), JSON results, and
`POLICY_EXECUTED` count.
JSON results appear only after all specs finish. If the run is interrupted,
use stderr's `[start]` and `[end]` lines to identify the last attempted spec;
an incomplete run is not a passing gate.

The Nix test gate uses `timeoutSeconds = 1800`. The host runner uses
`timeout -k 30s 1800s`: it sends the timeout signal after 1800 seconds and
forces termination after another 30 seconds if the process is still running.

## The gate

```sh
nix flake check --print-build-logs
```

The flake supports `x86_64-linux` and `aarch64-darwin`. The checks differ
by platform:

| Check | What it runs |
|---|---|
| `checks.x86_64-linux.default` | The full Nix test suite against the pinned dependencies. `CL_PROCESS_KIT_SPAWN` is set so the `bg` specs run, and `AITOOLS_E2E_BINARY` points `t/e2e/` at the delivered package. |
| `checks.aarch64-darwin.default` | Delivery artifacts: the host test runner and spawn helper must be executable, and the delivered binary's `schema` must return `status: ok` with nonempty, uniquely named commands. It does not run the full suite. |
| `checks.<system>.paredit-lint` | paredit-cli's structural lint over every Lisp source in the filtered distribution tree: delimiters must balance. It does not check layout. |
| `checks.<system>.formatting` | treefmt over the Nix sources. |
| `checks.<system>.docs` | `mkdocs build --strict` of this site. |

Darwin's native `ps` and `top` cannot run under the Nix build sandbox's
mandatory Seatbelt policy. On Darwin, `nix flake check` therefore checks
delivery artifacts, Lisp syntax, docs, and formatting; run `nix run .#test`
as well for the full suite on the host against that delivery.

CI (`.github/workflows/ci.yml`) requires both platform jobs:

| Runner | Required gates |
|---|---|
| `ubuntu-latest` (`x86_64-linux`) | `nix flake check --print-build-logs`, including the full Nix test suite. |
| `macos-15` (ARM, `aarch64-darwin`) | `nix flake check --print-build-logs` for delivery, syntax, docs, and formatting, then `nix run .#test --print-build-logs` for every spec on the host against the same flake's delivered binary. |

Both jobs also run `git diff --check` and have a 30-minute job timeout.
This overall limit includes setup and the Nix gate; it does not guarantee
the host suite a full 1800 seconds.

To include files git does not track yet, use the same `path:` flake reference
for both gates:

```sh
nix flake check path:. --print-build-logs
nix run path:.#test --print-build-logs
```

The distribution filter keeps `.lisp` and `.asd` files and the skill document,
but excludes scratch directories whose names start with `.release-probe.`,
`.session-audit-`, or `.store-fd-probe.`. Including a new test file does not
register its specs: add it to `aitools.asd` as described below.

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
  to the context's `src/` (for example `packages/feature/env/src/`), in load order.
  They go into the `aitools` system, not the repository-root `src/`.
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
   `application/` as a function that selects one supplied success or error
   callback and calls it once to return the result.
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
4. If the context needs new effects, add callable adapters to its ports
   structure and wire their real filesystem or process implementations in
   `make-production-<context>-ports` in `infrastructure/`. Application flows
   use those ports; tests can supply fake adapters.
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
