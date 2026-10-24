# Benchmarks

This page records only measurements that were taken, with the conditions
they were taken under. The repository has no CI timing benchmark suite; the
test suite includes one allocation check, described under
[Allocation](#allocation), and the manual adapter comparison below. No
sb-sprof profile of the commands has been recorded, and the cost of writing
journal metadata before a single-file write has not been measured.

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

## Digest and DEFLATE adapters

`t/perf/digest-deflate-adapter-benchmark.lisp` compares the pre-adapter implementations of SHA-256 and raw
DEFLATE with the current adapters. From the project root,
run it in the development shell:

```sh
nix develop --command sbcl --script t/perf/digest-deflate-adapter-benchmark.lisp
```

The harness reads the two baseline source files from revision
`1b20c3abdea1d95a049cdb0481afcfd72072b2a4` with `jj`, compiles them into a
temporary directory, and compares them with the loaded current systems in one
SBCL process. It uses deterministic xorshift inputs: 16 MiB for SHA-256 and
2 MiB for deflate and inflate. Inflate uses the shared stream produced by the
current deflater from the same 2 MiB input, so both decoders receive identical
bytes.

Each workload runs two warmup pairs and 15 measured pairs. The first
implementation alternates between pairs, and a full GC runs before every
invocation. The harness prints each paired sample and its relative change,
both arm medians, the median paired change, and the number of pairs where the
current adapter is faster. It also reports the SBCL version, platform,
workload sizes, and sample counts. Every pair checks SHA-256 parity against the
current digest, checks each deflate result by round-tripping it through both
decoders, or checks the inflate output and consumed length. Before measurement,
the harness registers this workspace with ASDF, directs compiled outputs into a
temporary workspace directory, and checks that the resolved source and
compiled output paths stay inside the workspace. These checks validate
results but impose no timing threshold; the harness is manual and is not part
of CI.

### Digest and DEFLATE results

Runs 3 and 4 are the valid measurements; Runs 1 and 2 failed during harness setup before timing (a package-reader error and a component lookup error) and were excluded. Both valid runs used the same Darwin ARM64 / Apple M4 Max host and SBCL 2.6.8, and each measured result passed the workload's correctness and parity checks. `Paired change` is `100 × (old − current) / old`; a positive value means the current adapter was faster. Medians are shown as old / current milliseconds.

| Workload | Payload | Run 3 medians | Run 3 paired change / current wins | Run 4 medians | Run 4 paired change / current wins |
|---|---:|---:|---:|---:|---:|
| SHA-256 | 16 MiB | 169.519 / 169.965 | +1.11% / 8 of 15 | 181.908 / 190.200 | −4.63% / 3 of 15 |
| deflate | 2 MiB input | 329.201 / 133.789 | +55.57% / 15 of 15 | 343.728 / 178.765 | +57.53% / 15 of 15 |
| inflate | 2 MiB decoded | 607.944 / 309.260 | +54.28% / 15 of 15 | 124.594 / 68.149 | +48.31% / 15 of 15 |

The deflate and inflate comparisons favored the current adapters in all 30 measured pairs for each workload. SHA-256 did not reproduce a direction: its paired median moved by 5.74 percentage points between runs and its faster-pair count changed from 8 of 15 to 3 of 15. These measurements do not establish that the current SHA-256 adapter is no slower. The run-to-run paired-median spread was 1.96 points for deflate and 5.97 points for inflate; no confidence interval was calculated. The arm medians also moved substantially, especially for inflate, so treat these as workload-specific advisory measurements rather than a general performance claim or a timing gate.

## Known dependency issues

These are issues aitools met in its dependencies. Current `flake.nix` pins
cl-regex-kit v2.2.0, cl-process-kit v3.4.0, cl-cli v1.4.0, and
cl-nix-forge v0.6.1. The last column summarizes how the current source
handles each issue.

| Dependency | Issue | Effect on aitools | Current behavior |
|---|---|---|---|
| cl-regex-kit | Before v2.1.1, a byte regex using `\b` cost time and memory proportional to the whole buffer on every scan, regardless of `:start` and `:end`. | `search` would pay whole-file cost on every candidate line. | `scan-line` (`packages/feature/search/src/domain/matcher.lisp`) scans the candidate line in place, bounded by `:end`, rather than copying it. |
| cl-regex-kit | Before v2.1.1, the Pike VM allocated per input byte for a pattern with no required literal. | Allocation grew with file size for such patterns. | None needed. The `[0-9]{5}` allocation case above is a normal passing spec. |
| cl-regex-kit | `(?:[^x]*r)?P` did not match `"P x"` in v2.1.0. | Broke the Rust `impl` pattern in the language table and a search matcher spec. | The matcher specs blocked on this now run as normal specs under v2.2.0. |
| cl-process-kit | Before v3.3.1, `spawn-native :session t` lost a setsid/process-group race on a fraction of Darwin launches. | `bg start` could fail before the program execs. | `bg start` launches once; a genuine failure is reported (`packages/feature/process/src/infrastructure/bg-launcher.lisp`). |
| cl-cli | `:rest-p` positionals consumed later options in v1.3.0. | `search` had to declare optional path positionals. | `search` uses one rest positional for paths under v1.4.0. |
| cl-nix-forge | Before v0.6.1, the `.asd` reader recursed once per character, so a `.asd` of a few tens of kilobytes exceeded Nix's default `max-call-depth`. | Evaluating `flake.nix`, which reads the version from `aitools.asd`, failed. | None needed. v0.6.1 walks the file iteratively. |
