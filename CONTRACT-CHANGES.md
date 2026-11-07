# Contract change list

## Foundation contracts

1. **Command declarations**: protocol application now exposes
   `define-command`, `command-declaration`, and declaration registration
   functions. A declaration carries a string name, optional group, command
   builder, and schema. `define-command` takes the command name and an options
   list with `:group`, `:command-builder`, and `:schema`. The separate
   `register-command-declarations` function receives the constructed dependency
   value. The context-registration macro
   `define-context-command-registration` accepts an opaque per-context
   dependency constructor; changing its returned value from ports to boundary
   objects does not change the registration contract.
2. **Continuation convention**: functions that can produce an `ok`, `partial`,
   or `error` result accept the canonical `:on-ok`, `:on-partial`, and `:on-error`
   keyword continuations. Positional continuation arguments are rejected. Pure
   functions that return one value remain direct-return functions and are not
   converted to CPS. The structural tests in `t/structure/cps-contract-*`
   enforce this convention.
3. **Test timeout**: the aitools test runner applies a 60-second default
   timeout to each registered cl-weave spec, including unit, integration, and
   e2e specs. External process calls retain explicit operation-specific
   timeouts.
4. **Operation deadlines**: external-process, HTTP, socket, `wait`, and
   `sys snapshot` operations have an overall deadline. Structure tests require
   timeout arguments on network calls and reject `process-kit:run` calls
   without `:timeout`. Detached background spawning is not represented by that
   API and remains the documented exception.
5. **Test support API**: common workspace, CLI, file/byte, tool discovery,
   and envelope matcher helpers are exported from `aitools.test.support`.
6. **Envelope/schema v2**: response envelopes use `schema_version: 2`; the
   shared matcher rejects version 1 and a missing version. There is no v1
   compatibility path. Success and error constructors accept optional
   `meta`, and either envelope may carry `recovered` after its payload.

## Reserved changes for later tracks

7. **Hash and archive APIs**: crypto-kit and deflate-kit adoption may replace
   internal implementations only after their required MD5, range, performance,
   `truncate-at`, `size-hint`, and archive-limit contracts are available.

## v0.4.0 downstream contracts

- **Application boundaries**: application code may use `cl-boundary-kit`
  boundary objects and fakes directly. Context-owned ports structures, such as
  `env-ports`, are migration targets and are removed as contexts adopt the
  boundary objects.
- **Definition macros**: use a definition macro when the same definition
  skeleton appears in three or more places. Do not introduce one for a skeleton
  used in only one or two places.
- **Coverage exclusions**: key each exclusion by kind, package, and symbol, and
  give it a reason. Structure tests fail when an exclusion no longer matches a
  current item.
- **Context-scoped tests**: select one context with
  `AITOOLS_TEST_CONTEXT=<ctx>` and the `aitools/test/<ctx>` system. For example,
  `AITOOLS_TEST_CONTEXT=env` selects `aitools/test/env` and that context's test
  count floor. Without the selector, the runner loads every context test system
  and the shared system.
- **Command registration order**: the feature presentation systems' order in
  `aitools/cli` dependencies determines registration order. An integration
  test checks that each declared feature context has one registration in that
  order.
- **Generated outputs**: regenerate the shared outputs with
  `docs/tools/generate-reference.lisp` to avoid parallel edits to generated files:
  `data/domain/protocol/correspondence-table-data.lisp`,
  the generated table in `docs/src/guide/agents.md`, and
  `docs/src/reference/commands/index.md`.

## Verification obligations for v0.4.0 downstream contracts

- Add an e2e or integration expectation for each downstream contract above before the v0.4.0 release.
- Tests must reject the old envelope shape.
- Envelope version 2 is the active contract for this migration.

## Structured document commands

- The inspect and edit contexts each declare one `doc` command for each of
  `get`, `select`, `diff`, `set`, `delete`, `merge`, `patch`, and `fmt`.
- The application flow resolves a document's format from `--format` first,
  then from the path extension. `diff` resolves each path independently unless
  `--format` overrides both. Missing or unsupported extensions fail with
  `argument.invalid`; content sniffing is not used.
- Format codecs convert documents to and from the shared JSON value model.
  Read and edit operations use that shared model; YAML/TOML comment warnings,
  TOML ordering, and JSON-only `fmt` options remain at their format-specific
  boundaries. JSON-only options fail with `argument.invalid` for YAML or TOML.
