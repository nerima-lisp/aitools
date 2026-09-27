# Benchmarks

This page records only measurements that were taken, with the conditions
they were taken under. The repository includes a committed allocation
benchmark suite; no sb-sprof profile of the commands has been recorded, and
the cost of writing the intent record for a single-file write has not been
measured.

## Startup

The recorded baseline on aarch64-darwin, measured with hyperfine
(warmup 3):

| Command | Mean |
|---|---|
| `sbcl --non-interactive --eval '(sb-ext:exit)'` (SBCL 2.6.0) | 26.7 ms |
| `rg --version` | 7.5 ms |

`aitools --version` startup has not been measured on an idle host. The one
figure that exists was taken during the search comparison below, under heavy
host load, so it is not reproducible; re-measure with hyperfine on an idle
host before quoting a number.

## `search` compared with ripgrep

No reproducible comparison exists yet. The figures the search implementation
run reported were taken on aarch64-darwin under a host load average of 115 to
153 on 16 CPUs, with other Nix builds running, so the wall times are noisy
and are not recorded here. The exact hyperfine command lines were not
recorded either. Match counts were identical to ripgrep's for all five
patterns tested.

To produce a comparison, run `aitools search` and `rg` under hyperfine on an
idle host over the same corpus (the earlier run used 1000 files, 39 MB,
generated with seed 42 outside the worktree) and record the exact command
lines alongside the result.

## Allocation

`t/perf/search-allocation-test.lisp` measures bytes consed (a deterministic
count, not time) by the `search` flow on a file of N non-matching lines plus
one matching line. Each measurement is the least of five runs after two
warm-up runs, because the counter is process-wide and a worker thread
allocating during a run inflates a single sample. A case passes when the
difference between N = 1,000 and N = 100,000 stays under 65,536 bytes; one
cons cell per line would add about 1.6 MB. It runs for these cases:

- `needle`, `needle --ignore-case`, `need[a-z]e --word`,
  `needle --output count`, `needle --output matches`
- `[0-9]{5}`, a pattern with no required literal, so every line reaches the
  regex engine
- the per-file matcher alone, comparing 1,000 and 100,000 non-matching lines;
  the allocation difference must stay below 4,096 bytes

To run the suite, including this file, see
[Development](../project/development.md#running-the-tests).

## Known dependency issues

These are issues aitools met in its dependencies, at the versions
`flake.nix` pins: cl-regex-kit v2.1.1, cl-process-kit v3.3.1, cl-cli v1.4.0,
cl-nix-forge v0.6.1. The last column says whether aitools still carries a
workaround in the current source.

| Dependency | Issue | Effect on aitools | Workaround |
|---|---|---|---|
| cl-regex-kit | Before v2.1.1, a byte regex using `\b` cost time and memory proportional to the whole buffer on every scan, regardless of `:start` and `:end`. | `search` would pay whole-file cost on every candidate line. | Removed. `scan-line` (`packages/feature/search/src/domain/matcher.lisp`) scans the candidate line in place, bounded by `:end`, instead of copying it. |
| cl-regex-kit | Before v2.1.1, the Pike VM allocated per input byte for a pattern with no required literal. | Allocation grew with file size for such patterns. | None needed. The `[0-9]{5}` allocation case above is a normal passing spec. |
| cl-regex-kit | `(?:[^x]*r)?P` did not match `"P x"` in v2.1.0. | Broke the Rust `impl` pattern in the language table and a search matcher spec. | Removed. The search matcher specs blocked on this are normal specs under v2.1.1. |
| cl-process-kit | Before v3.3.1, `spawn-native :session t` lost a setsid/process-group race on a fraction of Darwin launches. | `bg start` could fail before the program execs. | Removed. `bg start` launches once; a genuine failure is reported (`packages/feature/process/src/infrastructure/bg-launcher.lisp`). |
| cl-cli | `:rest-p` positionals consumed later options in v1.3.0. | `search` had to declare optional path positionals. | Removed. `search` uses one rest positional for paths under v1.4.0. |
| cl-nix-forge | Before v0.6.1, the `.asd` reader recursed once per character, so a `.asd` of a few tens of kilobytes exceeded Nix's default `max-call-depth`. | Evaluating `flake.nix`, which reads the version from `aitools.asd`, failed. | None needed. v0.6.1 walks the file iteratively. |
