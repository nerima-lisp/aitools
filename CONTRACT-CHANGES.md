# Contract change list

This list records the contracts approved by the refactoring specification.
Context tracks must follow these contracts and reflect them in tests and
documentation.

## W0 foundation contracts

1. **Command declarations**: protocol application now exposes
   `define-command`, `command-declaration`, and declaration registration
   functions. A declaration carries a string name, optional group, command
   builder, and schema; builders receive the context ports value.
2. **Command continuations**: command flows are invoked with the canonical
   `:on-ok`, `:on-partial`, and `:on-error` keyword continuations. Positional
   continuation arguments are rejected.
3. **Test timeout**: cl-weave specs use a 30-second default timeout. The test
   runner raises its integration/e2e budget to 60 seconds while external
   process calls retain explicit operation-specific timeouts.
4. **Process timeout gate**: structure-test rejects source forms calling
   `process-kit:run` without `:timeout`. Detached background spawning is not
   represented by that API and remains the documented exception.
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

## Verification obligations

- Every item must have an e2e or integration expectation before release.
- Tests must reject the old envelope shape.
- Envelope version 2 is the active contract for this migration.
