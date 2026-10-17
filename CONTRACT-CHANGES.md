# Contract change list

This list records the W0 contract changes approved by the refactoring
specification. It is the hand-off for context tracks and must be reflected in
the relevant tests and documentation before release.

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

## Reserved changes for later tracks

6. **Envelope/schema**: if context refactors change the JSON envelope, the
   schema version becomes `2`; existing schema version `1` is retained until
   the e2e expectations and documentation change together.
7. **Hash and archive APIs**: crypto-kit and deflate-kit adoption may replace
   internal implementations only after their required MD5, range, performance,
   `truncate-at`, `size-hint`, and archive-limit contracts are available.

## Verification obligations

- Every item must have an e2e or integration expectation before release.
- The old output shape must not be accepted by tests once a new envelope is
  selected.
- This file is included in the W0 pull request body and handed to D1/V1.
