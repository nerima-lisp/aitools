# Benchmarks

This page records only measurements that were taken, with the conditions
they were taken under. The repository has no CI timing benchmark suite; the
test suite includes one allocation check, described under
[Allocation](#allocation), and the manual adapter comparison below. No
`sb-sprof` profile was taken for the startup, search, or adapter runs described
here. The journal overhead for a one-file `edit` operation has not been
measured.

## Startup

The recorded baseline on aarch64-darwin, measured with hyperfine
(warmup 3):

| Command | Mean |
|---|---|
| `sbcl --non-interactive --eval '(sb-ext:exit)'` (SBCL 2.6.0) | 26.7 ms |
| `rg --version` | 7.5 ms |

`aitools --version` startup has not been measured on an idle host. The only
recorded `aitools --version` figure came from the search comparison below, under
heavy host load, so it is not reproducible; re-measure with hyperfine on an idle
host before quoting a number.

## `search` compared with ripgrep

No reproducible comparison exists yet. A search implementation run reported
wall times on aarch64-darwin under a host load average of 115 to 153 on 16 CPUs,
with other Nix builds running, so the times are noisy and are not recorded
here. The exact hyperfine commands and five tested patterns were not recorded.
Match counts were identical to ripgrep's for all five tested patterns.

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
DEFLATE with the current adapters. From the project root, run the comparison
and current/current control in separate development-shell invocations:

```sh
nix develop --command sbcl --script t/perf/digest-deflate-adapter-benchmark.lisp --mode compare --warmup-pairs 6 --sample-pairs 80
nix develop --command sbcl --script t/perf/digest-deflate-adapter-benchmark.lisp --mode self --self-arm current --warmup-pairs 6 --sample-pairs 80
```

The harness reads the two baseline source files from revision
`1b20c3abdea1d95a049cdb0481afcfd72072b2a4` with `jj`, compiles them into a
temporary directory, and compares them with the loaded current systems in one
SBCL process. That revision must be available in the local `jj` history. Each
invocation measures all three workloads in one fresh SBCL process. It uses
deterministic xorshift inputs: 16 MiB for SHA-256 and 2 MiB
for deflate and inflate. Inflate uses the shared stream produced by the current
deflater from the same 2 MiB input, so both decoders receive identical bytes.

Each workload discards six warmup pairs and measures 80 pairs. In comparison
mode, each pair runs both the baseline and current implementation, alternating
which runs first. In self mode, both arms run the current implementation with
the same alternating order. A full GC runs before every invocation. The harness
prints paired times to six decimal places, relative changes, output signatures,
medians, second-arm win counts, and a distribution-free median interval. Every pair
checks its workload's correctness: SHA-256 parity against the current digest,
deflate round trips through both decoders, or inflate output and consumed
length. Before measurement, the harness registers this workspace with ASDF, directs compiled
outputs into a temporary workspace directory, and checks that the resolved
source and compiled output paths stay inside the workspace. These checks
validate results but impose no timing threshold; the harness is manual and is
not part of CI.

### Digest and DEFLATE results

The measurements below used Darwin ARM64 / Apple M4 Max and SBCL 2.6.8. Each
comparison run loaded baseline revision
`1b20c3abdea1d95a049cdb0481afcfd72072b2a4` and the current adapter in the same
process. Both A/A runs used the current adapter in both arms. All 80 measured
pairs per workload passed their correctness checks. SHA-256 and inflate output
signatures matched across the A/B arms; deflate outputs were validated by both
decoders but had different compressed-byte signatures. An A/A pair difference
is `100 × (time_b − time_a) / time_a`. The A/A noise width is the 95th
percentile of the absolute differences pooled across 160 current/current pairs,
using the 152nd ordered value. The A/B table reports old/current arm medians
and the median of 80 values of `100 × (current − old) / old` for each run;
positive values mean current was slower. It also reports current wins and the
distribution-free median interval from the 31st and 50th ordered pair
differences. These order-statistic intervals assume independent paired
differences and do not adjust for temporal correlation in system load.

| Workload | Payload | A/A noise width, 160 pairs |
|---|---:|---:|
| SHA-256 | 16 MiB | 69.013762% |
| deflate | 2 MiB input | 70.810413% |
| inflate | 2 MiB decoded | 113.286804% |

| Workload | Run | Old / current median (ms) | Paired median change | 95%+ interval | Current faster |
|---|---:|---:|---:|---:|---:|
| SHA-256 | 1 | 252.697995 / 245.552500 | +3.516982% | −1.350893% to +5.898093% | 32/80 |
| SHA-256 | 2 | 239.638000 / 235.380495 | −0.663535% | −3.210751% to +3.869691% | 42/80 |
| deflate | 1 | 490.143010 / 185.922500 | −60.222820% | −63.529202% to −57.141015% | 80/80 |
| deflate | 2 | 355.699000 / 147.624495 | −57.826465% | −59.288730% to −56.175170% | 80/80 |
| inflate | 1 | 140.536005 / 74.548500 | −47.002578% | −48.325899% to −45.224114% | 78/80 |
| inflate | 2 | 174.507010 / 87.870500 | −47.208156% | −50.088415% to −43.256759% | 74/80 |

For SHA-256, both A/B interval upper bounds are inside the measured A/A noise
width, so these runs support the conclusion that the current adapter was not
slower than the baseline within the resolution of this measurement. The A/A
noise width is large, so this is not evidence of tight performance equivalence.
Deflate and inflate were faster with the current adapters in both runs, with
both intervals below zero.

The host was under high load from another project's processing during every
official run. The recorded one-minute load averages immediately before and
after each process were 75.66 to 93.87 for A/A run 1, 90.58 to 60.04 for A/A
run 2, 55.20 to 60.61 for A/B run 1, and 62.26 to 71.75 for A/B run 2. Each
run's `uptime` output
and all raw paired samples were retained with the local measurement logs under
`~/.local/state/aitools-phase2/logs/`: `sha256-perf-aa-current-run1.log`,
`sha256-perf-aa-current-run2.log`, `sha256-perf-ab-run1.log`, and
`sha256-perf-ab-run2.log`. The high-load condition is part of these results and
makes the measured noise widths poor thresholds for detecting small
regressions. These are advisory measurements, not a general performance
guarantee or a CI timing gate.

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
