# aitools

aitools is a command-line tool for AI agents with built-in file and text
operations and a runner for installed programs (`awk`, `sed`, `grep`, and
others). People are not its audience: it has no color, prompts, or shell
completion.

- **One JSON object per call.** Success goes to standard output, errors go to
  standard error. Exit code 0 marks success; 2 marks a precondition failure,
  3 a partial result, and 1 another error.
- **Bounded reads.** Every read has a limit. A cut result says so and
  carries the command that reads the next part.
- **Guarded writes.** A guarded edit names the text it replaces or the hash and
  count it expects, and writes nothing when the file differs.
- **Journaled writes with recovery and undo.** On a later run, recovery finishes
  or discards an interrupted write when safe. If a checkpoint cannot be
  reopened safely, it retains the intent for operator recovery. Use
  `aitools undo <op_id>` to revert a completed operation.
- **Transactions.** Writes from several calls can be staged, inspected, and
  committed as one operation.
- **Secret masking.** Known secret formats are replaced with
  `[REDACTED_SECRET]` in JSON output.
- **External programs.** `aitools run -- PROGRAM ARG...` captures output and
  reports the child's exit status. External writes are not guarded or
  journaled; raw output saved with `--stdout-to` is not secret-masked.
- **Repairs.** Every error includes at least one suggested aitools command.
  Review it before use; some repairs name only a command, without the arguments
  needed for a complete invocation. Calling a shell command name such as `cat`
  returns a suggested aitools replacement.

aitools is written in Common Lisp for SBCL and built with Nix.

## Where to go next

- [Getting started](getting-started.md): build the binary and run the first
  commands.
- [Using aitools from an agent](guide/agents.md): the call loop, guards,
  transactions, and the shell-command correspondence table.
- [Commands](reference/commands/index.md): every command's arguments and output.
- [Architecture](reference/architecture.md): contexts, layers, ports, and the
  CPS audit.

These pages are the specification of aitools's behavior.
[Commands](reference/commands/index.md#not-provided) lists the options and
commands that are not provided.
