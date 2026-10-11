# State inspection

Choose the observation scope before interpreting an empty result. No command
enumerates every machine state, and separate observations can race with
other processes. An unavailable source is not evidence that nothing exists.

| Scope | Command | Coverage and limits |
|---|---|---|
| Host resources | `sys snapshot [PATH]` | Info, CPU/memory/process metrics, interface counters, one filesystem, and TCP listeners. Sequential, non-atomic observations with per-section status and times. |
| Environment | `sys env [PREFIX]` | Current process environment with secret-name and known-format masking; not a guarantee that arbitrary secrets are absent. |
| Installed tools | `sys tools [NAMES...]` | PATH lookup and version probes; probing runs the programs. |
| Processes | `sys procs [PATTERN]` | Command-line substring matches ordered by PID; a limit can omit matches. Unlike metrics, this includes command lines. |
| Workspace layout | `overview [PATH]` | File/language/build overview and selected Git metadata; not every modification or filesystem attribute. |
| Git changes | `git status` | Git working-tree state; not a substitute for jj's revision or operation state. |
| Managed jobs | `bg status [ID]` | Jobs registered in this workspace, not every host process. List results identify unreadable or invalid records in `omitted`. |
| Job output | `bg logs ID` | Bounded log windows; follow the returned offset to continue. |
| Transactions | `tx status [ID]`, `tx diff ID` | Open staged operations, recorded reads, drift, and a bounded proposed diff. |
| Committed writes | `history --limit 20` | Aitools journal operations only, not external program writes. When truncated, follow `next_commands` to rerun with a larger limit; this is not offset pagination. |
| File baseline | `snapshot create`, `snapshot diff` | Persistent workspace comparison, separate from `sys snapshot`; creating the baseline writes state. |

Use `aitools schema COMMAND` for arguments and output fields, for example
`aitools schema bg status`.

## Collect workspace observations

To request several independent observations in one invocation:

```sh
printf '%s\n' '[["overview"],["bg","status"],["tx","status"],["history","--limit","20"]]' |
  aitools batch --stdin --continue-on-error
```

Batch runs commands sequentially, not atomically. `--continue-on-error`
collects later results after a child fails. Inspect each result's status and
truncation fields: a partial child can coexist with a successful batch exit.
Without this option, a failure skips the remaining commands. Batch has no
overall acquisition or output budget.

## Interpret incomplete observations

For `sys snapshot`, inspect `sections`, `limits`, and `collection_bounds`.
The disk section observes the filesystem containing `PATH`, or the workspace
root when omitted. `PATH` does not change the info section's workspace data.
Missing socket owners can reflect permission limits. Network counters do
not describe DNS, routes, established connections, or reachability. Disk
capacity does not describe I/O throughput, health, or every mounted volume.

On Linux, inspect socket `coverage` and `source_unavailable`. If either TCP
family source is missing, the ports section is partial even with no returned
listeners. If both are missing, it is unavailable. Neither result establishes
that the host has no listeners.

For `bg status`, `total` counts valid returned records and `observed_total`
counts identifiers seen during listing. Each omitted identifier has a reason;
an all-invalid listing is partial, not a normal empty list. Concurrent job
changes can still make observations stale immediately after collection.

Normal command dispatch can perform journal recovery before the requested
handler runs. These commands do not provide a pre-recovery forensic view;
failed recovery can prevent the observation. External filesystem writes,
permissions, ignored paths, and command-specific limits also constrain
coverage.
