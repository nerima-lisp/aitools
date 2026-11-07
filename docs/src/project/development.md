# Development

## Running the tests

Start the development shell:

```sh
nix develop
```

Inside that shell, run the full suite, one context, or full coverage:

```sh
sbcl --script run-tests.lisp
AITOOLS_TEST_CONTEXT=search sbcl --script run-tests.lisp
AITOOLS_COVERAGE=1 sbcl --script run-tests.lisp
```

Outside it, set `CL_SOURCE_REGISTRY` to a parent directory containing a
checkout directory for each dependency. For example, put these pinned
checkouts below `$HOME/src/nerima-lisp`:

- Direct dependencies: cl-cli, cl-weave, cl-json-kit, cl-regex-kit,
  cl-codec-kit, cl-host-kit, cl-boundary-kit, cl-concurrent-kit,
  and cl-process-kit.
- Transitive dependencies: cl-date-kit, cl-log-kit, and cl-parser-kit.

Use the versions pinned in `flake.nix`.

```sh
export CL_SOURCE_REGISTRY="$HOME/src/nerima-lisp//:"
sbcl --script run-tests.lisp
```

The `//:` suffix makes ASDF search the dependency trees below that directory.
`run-tests.lisp` adds this checkout to ASDF automatically, then
loads `aitools` and runs `asdf:test-system "aitools/test"`. That operation
discovers registered `aitools/test/<context>` systems through ASDF. With
`AITOOLS_TEST_CONTEXT` set, it loads only that context's tests; without it, it
loads every context system and `aitools/test/shared`.
`run-tests.lisp` exits 1 with a one-line message if the ASDF test operation
signals an error, and exits 0 otherwise. The policy in `t/package.lisp`
rejects failures, errors,
too few executed specs, and skips or todos outside its explicit allowlist.
The per-spec report printed to stdout includes the executed count and any
allowed unrun specs.
`AITOOLS_COVERAGE` always measures the full test set and cannot be combined
with `AITOOLS_TEST_CONTEXT`.

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

The Linux `checks.x86_64-linux.default` test gate uses
`timeoutSeconds = 1800`. The host runner uses
`timeout -k 30s 1800s`: it sends the timeout signal after 1800 seconds and
forces termination after another 30 seconds if the process is still running.

## Flake checks

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

The distribution filter keeps `.lisp` and `.asd` files plus
`skills/aitools/SKILL.md`, but excludes scratch directories whose names start
with `.release-probe.`, `.session-audit-`, or `.store-fd-probe.`. Including a
new test file does not register its specs: add it to its test system as
described below.

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
t/unit/<context>/     domain and application specs with fake dependencies
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

Each context's ASD under `packages/` owns its library and test systems.
`aitools.asd` loads those ASD files and
composes the root systems; it also owns the shared data system and CLI
composition root. The production components, test components, and count-floor
files are:

- **library** files (`domain/`, `application/`, `infrastructure/`), relative
  to each context's `src/` (for example `packages/feature/env/src/`), in that
  context's ASD. The root `aitools` system depends on the context systems.
- **presentation** files, also under the context's `src/`, are registered by
  context ASDs and loaded through `aitools/cli`.
- **data** files, relative to the repository's `data/`, are listed in the
  shared `aitools/data` system in `aitools.asd`. They load before the library
  systems into the shared `aitools.data` package.
- **test** files, relative to `t/`, are listed under `aitools/test/<context>`
  in their context ASDs. Shared package and support files live in
  `aitools/test/base`; shared, structural, integration, and end-to-end tests
  live in `aitools/test/shared`. The root `aitools/test` system discovers
  registered context systems dynamically.
- **spec-count floors** live in `t/spec-count-baselines/<context>.lisp` and
  are checked by the shared `t/spec-count-baselines/loader.lisp`.

The contexts appear in load order: core kernel, protocol, workspace, text,
store, then feature search, inspect, journal, edit, process, env, util.
A context loads after every context whose application package it calls:
journal before edit (edit registers its `tx rebase` replayers with journal),
inspect before edit and edit before util.

To add or move context library or presentation files, edit that context's ASD.
Update `aitools.asd` when adding shared data or changing root system
dependencies. To add or move context tests, edit that context's
`aitools/test/<context>` components in its ASD. Shared test components belong
in `aitools-test-system.lisp`.

## Adding a command

1. Put the logic in the owning context: pure rules in `domain/`, the flow in
   `application/` as a function that accepts `:on-ok`, `:on-partial`, and
   `:on-error` continuations, then calls exactly one with the result fields.
2. In `presentation/`, define command builders and schemas, then pass command
   declarations to `aitools.protocol.application:register-command-declarations`
   from `register-<context>-commands (registry dependency-value)`:

    ```lisp
    (aitools.protocol.application:register-command-declarations
     registry
     (list
      (aitools.protocol.application:define-command
       "doc.get"
       (:group "doc"
        :command-builder #'%get-command
        :schema (%schema "doc.get"))))
     dependency-value)
    ```

    `define-command` takes the command name and its options. Its command
    builder receives the dependency value and produces the cl-cli command;
    `register-command-declarations` applies the declaration's group and schema.
3. For a new feature context, define a dependency constructor in its
   `presentation/context-registration.lisp` and declare the context with
   `aitools/cli::define-context-command-registration`, passing the context
   name, `register-<context>-commands`, and the constructor. Existing contexts
   use this same registration file; update their constructor when a command
   needs new effects. The context owns the constructor's input contract and
   returned dependency value; consult its signature for inputs. That signature
   can change without changing the registration macro contract. The macro
   stores the constructor without inspecting its inputs or result. The
   composition root passes the constructor's returned value to the registration
   function unchanged.
4. Each command builder returns a cl-cli command with a handler. The handler
   returns a command result for dispatch by calling the application flow through
   `aitools.protocol.application:call-with-command-result/k`. That function
   returns the command result produced by the flow's one continuation. If the
   flow needs a new effect, add an effect
   function to the context dependency value. The dependency constructor combines
   production effect functions in infrastructure; tests provide fake functions
   through a context-owned value with the same contract.
5. Put static tables shared across files or consumed by reference generation
   in `data/<layer>/<context>/*-data.lisp`. Keep computed values and behavior
   in their owning layer.
6. Add specs under `t/`. List production files in the context's ASD components
   and tests in its `aitools/test/<context>` system. Shared tests go in
   `aitools-test-system.lisp`. [Regenerate the reference](#regenerating-the-reference)
   when a command schema changes.

## Regenerating the reference

The command index and context pages under [Commands](../reference/commands/index.md),
the error table in [Errors](../reference/errors.md), and the correspondence
table in [Using aitools from an agent](../guide/agents.md) are generated.
Regenerate them after changing a command schema, the error catalog, or
`data/domain/protocol/correspondence-table-data.lisp`:

```sh
sbcl --script docs/tools/generate-reference.lisp
sbcl --script docs/tools/generate-reference.lisp --check
```

The `--check` form fails when generated text is stale without rewriting files.

It needs the same `CL_SOURCE_REGISTRY` as `run-tests.lisp`. It loads
`aitools/cli` and calls each context's `register-<context>-commands` into a
fresh registry. It rewrites marked regions and the generated context pages.
Review the diff before committing.
