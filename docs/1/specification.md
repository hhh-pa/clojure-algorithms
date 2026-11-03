# 1: callgrind perf CI with JVM vs clojure-rs comparison

## Part 1 — goal and acceptance criteria

Goal: measure every test namespace under deterministic callgrind
for both JVM Clojure and clojure-rs
(`clojure-rs`, https://github.com/hhh-pa/clojure) — so
`callgrind_instructions` and `callgrind_allocated_bytes` for the
two runtimes overlay per benchmark on Bencher's perf page (same
benchmark names, distinct testbeds). This mirrors the snippet
repo's T1211 sweep (hhh-pa/clojure-code-snippet#37), adapted
from script-x-case benchmarks to test-namespace benchmarks.

Acceptance criteria:

- New `perf-callgrind.yml` runs a shard(8) x
  runtime{jvm, clojure-rs} matrix; `report-callgrind` merges
  per runtime; `ci.yml` `test` is unchanged.
- `algoperf measure --runtime clojure-rs --command-template
  'clojure-rs run perf/driver.clj {ns} src test resources' --callgrind`
  reports both gungraun-run measures per namespace (valgrind
  Ir + allocator-shim total); the JVM recipe behaves exactly
  like T1211 (same flags, same parsers, same BMF shape).
- The per-namespace correctness gate is the SUMMARY line:
  `:fail`/`:error` zero on both legs, and the report fails
  when any namespace's counts differ between legs.
- `algoperf measure --self-test` reports 0 failed; `go test
  ./...` is green; `python3 -c yaml.safe_load` passes on the
  workflow; `git diff --check` is clean.
- The pushed branch shows green `test` + all 16 sweep legs +
  both `report-callgrind` merges, with per-shard Bencher
  reports for both runtimes and merged tables in the report
  summaries.
- Every per-shard Bencher check stays under GitHub's ~64KB
  summary limit.

BDD scenarios (user-observable):

- Given a push to the branch, when CI runs, then the
  correctness gate in `test` stays green, and 16 sweep legs
  measure deterministic instruction and allocation counts
  for both runtimes over all 1351 test namespaces.
- Given Bencher's perf page, when the `ubuntu-latest-jvm`
  and `ubuntu-latest-clojure-rs` testbeds are overlaid, then
  each namespace shows JVM vs clojure-rs Ir and allocated
  bytes side by side.

Non-goals:

- No `--slope` in the sweep (unchanged from T1211: slope
  fits timed runs; the sweep stays lean) and no AOT leg for
  clojure-rs (`clojure-rs build` comparison is future work).
- No timed `allocated_bytes` for the native runtime: it
  stays null, exactly like the JVM sweep under `--no-alloc`.
  Native allocation is reported only as
  `callgrind_allocated_bytes`.
- No full-stdout diff gate: namespaces print
  nondeterministically (timing, random samples, hash order),
  so the SUMMARY line is the gate (see design).
- No changes to `deps.edn` or `build.clj`. The `src/` +
  `test/` changes on this branch are limited to the four
  test-isolation fixes the sweep itself surfaced (three
  missing `:require`s that only passed by ambient load
  order, plus a fail-fast negative-exponent guard replacing
  a stack-exhaustion dependency — the underlying
  abort-vs-`StackOverflowError` port gap is tracked at
  hhh-pa/clojure#220).
- No Bencher-side threshold tuning past the T1211 1%
  `callgrind_instructions` policy; no branch-protection edit
  on this branch — both are merge-time admin steps (below).

Merge prerequisites (repo admin, at merge time — not on this
branch):

- Create the Bencher project `clojure-algorithms` and set
  the `BENCHER_API_KEY` repository secret (per-shard
  `bencher run` reports against it; without the secret the
  Bencher steps fail — keep the PR in draft until the
  secret exists, or accept red Bencher steps as known).
- Optionally require the new `perf-callgrind (...)` /
  `report-callgrind (...)` checks in branch protection
  (exact names taken from this PR's checks).

## Part 2 — design

### Benchmarks: one per test namespace

1351 files match `test/**/*_test.clj`, and every file's `ns`
decl equals its path mapped by `test/`-strip, `/` -> `.`,
`_` -> `-` (verified programmatically over all 1351).
Benchmark names are the ns symbols — identical on both legs.

### Driver (`perf/driver.clj <ns> [load-path...]`)

Same driver both legs (identical workload):

```clojure
(when-let [lp (seq (rest *command-line-args*))]
  (alter-var-root #'clojure.core/*load-path*
    (constantly (vec lp))))
(require (symbol (first *command-line-args*)))
(let [sum (clojure.test/run-tests
            (symbol (first *command-line-args*)))]
  (println "SUMMARY" (select-keys sum [:test :pass :fail :error])))
```

- JVM leg passes no load-path (the `-cp` covers `src`/`test`);
  the native leg passes `src test resources` (verified: require +
  run-tests + SUMMARY all work under `clojure-rs run`).
- No `System/exit` (unemulated on the native leg): the gate
  parses the SUMMARY line and fails the case on `:fail`/`:error`
  > 0 or a missing SUMMARY (require crashes exit non-zero
  with no SUMMARY — same treatment).
- Proven outputs match: `{:test 2, :pass 10, :fail 0,
  :error 0}` on both runtimes for the probe namespace.

Invocations:

- JVM: `java -Duser.language=en -Duser.country=US -cp
  '<jars>:src:test:resources' clojure.main perf/driver.clj
  <ns>` (the locale pin keeps `format`-based assertions and
  instruction counts host-independent; jars: clojure 1.12.0 +
  spec.alpha
  0.5.238 + core.specs.alpha 0.4.74, Maven-cached like the
  snippet CI — no Clojure CLI needed, test-runner not
  needed since the driver calls `clojure.test` directly).
- Native: `clojure-rs run perf/driver.clj <ns> src test resources`.

### SUMMARY gate + cross-leg comparison

- Per leg, per namespace: exit 0 + SUMMARY parsed +
  `:fail`/`:error` zero, else the case fails loudly.
- At `report-callgrind` time: diff the per-namespace
  `(test,pass,fail,error)` tuples JVM vs clojure-rs (a
  dedicated `report-compare` job diffs the recorded
  `test_counts` maps with jq); any mismatch fails the
  report (behavioral drift can never hide behind perf
  numbers). Two namespaces (`graphs.karger-test`,
  `graphs.random-graph-generator-test`) assert per edge of
  a random graph, so their `:pass` counts vary by design
  and are normalized before the diff (`:test`/`:fail`/`:error`
  still compare exactly).
- Timeouts are split: `--timeout` (default 1800s) budgets
  the native-speed runs (whose SUMMARY gate must stay
  decisive — the slowest namespace needs ~11min on the
  native leg), while `--callgrind-timeout` (default 600s)
  budgets the instrumented phase, which degrades to
  `callgrind_skipped` with a reason (visible in JSON,
  excluded from BMF) — the T1211 policy. Tune from CI data.

### `algoperf measure`: vendored recipe, native discovery

New Go module `github.com/hhh-pa/clojure-algorithms/
scripts/ci/go` with `cmd/algoperf` + a vendored
`internal/measure` subset, ported line-for-line from the
snippet repo's `scripts/ci/go/internal/measure`:

- Keep (recipe-identical): valgrind callgrind flags, the
  JVM `-Xint` + EpsilonGC splice, the LD_PRELOAD
  counting-shim source and parser, the callgrind summary
  parser, BMF emission, shard selection.
- Drop: `--aot` / `AOTCache` (aot-only), `--slope` /
  complexity BMF (slope-only), snippet discovery/resources.
- Vendor alongside: `pyjson` (whole, small),
  `snipcore` helpers actually used (`Run`/`RunEnv`,
  `Normalize`, `PyRepr`, `TruncateChars`, `LastLine`,
  `TimeoutError`, `ExitCode`).
- New: namespace discovery (walk `test/`, ns mapping),
  SUMMARY parser + gate, thin CLI (`measure`, `--merge`,
  `--self-test`), own self-tests (SUMMARY parser checks
  first, following the `ParseCallgrindSummary` pattern).
- Semantic note (documented in README): the JVM number is
  EpsilonGC heap-used-at-exit, the native number is
  malloc-request bytes — ratio-meaningful per benchmark,
  not bit-comparable absolutes (T1211 wording).

### Workflow (`perf-callgrind.yml`)

- `resolve-rs`: resolve clojure-rs master HEAD once per
  run (float; manual dispatch may pin `clojure_rs_sha`,
  40-char SHA validated) — same as the snippet repo.
- `perf-callgrind` matrix: `shard: [0..7]` x
  `runtime: [jvm, clojure-rs]` (16 legs).
  - JVM leg: pinned temurin 21.0.13 (deterministic
    history) + cached Maven jars.
  - Native leg: stable Rust (`dtolnay/rust-toolchain`),
    checkout `hhh-pa/clojure` at the resolved SHA, `cargo
    build --release --locked --no-default-features` in
    `rust/` (interpreter only, no LLVM), `Swatinem/rust-cache`,
    binary to `$GITHUB_WORKSPACE/cljrs-bin` + `$GITHUB_PATH`.
  - Measure step: `--repeat 1 --no-alloc --callgrind
    --callgrind-repeat 1 --jobs 2 --timeout 600
    --shard N/8`, runtime template per leg.
  - Bencher step: T1211 threshold flags verbatim, project
    `clojure-algorithms`, testbeds
    `ubuntu-latest-{jvm,clojure-rs}`; `jq` strip +
    artifact upload runtime-agnostic (names carry
    runtime+shard).
- `report-callgrind` matrix over runtimes: merge +
  `jq -s add` verify + SUMMARY cross-leg diff + upload,
  merged tables in the summary.

Shard math: namespaces are ms-scale test logic (max 175
lines/file) behind runtime startup (rs 0.07s, JVM 0.37s
native locally), the same shape as snippet cases
(~21s all-in): 1351 x ~25s / 2 jobs / 8 shards ~= 35
min/shard. 16 shards stay available as a future lever;
per-shard BMFs (~170 benchmarks x 2 measures) sit far
below the check-summary limit.

### TDD plan

- SUMMARY parser + gate checks first (valid line amid
  noise, missing/failing SUMMARY -> error), then the
  vendored recipe files with the snippet self-tests
  adapted (parser checks stay subprocess-free; live
  shim/valgrind checks `t.Skip` where `cc`/Linux/valgrind
  are absent).
- Local gates: `go build`, `go vet`, `go test ./...`,
  `algoperf measure --self-test`, plus a native-leg
  end-to-end over a few namespaces (`--only`, no
  callgrind — this host has no valgrind) and the JVM
  driver run (java + jars work locally).
- Linux CI on the PR is the proof for the valgrind/shim
  paths (same as T1211).
- Workflow validated by `python3 -c yaml.safe_load` and
  a matrix/condition review.

### CI impact

- New workflow file only; `ci.yml` untouched, no
  Jenkinsfile (this repo uses GitHub Actions).
- New CI dependencies on the native leg only: Rust
  stable toolchain + `Swatinem/rust-cache` (no LLVM);
  JVM leg adds a Maven jar cache.

### Risks

- `BENCHER_API_KEY` does not exist yet: Bencher steps
  fail until the admin step lands (draft PR meanwhile).
- Heavy namespaces (blockchain, SMO, primelib) may blow
  the 600s timeout under callgrind on the first sweep —
  they degrade to skips with reasons, then the timeout
  tunes from data.
- clojure-rs nondeterminism (hash seeds) surfaces as
  SUMMARY-mismatch failures or Bencher wobble, never as
  silent drift; the 1% threshold arms after 10 samples.
- If native callgrind runs slower than estimated, the
  lever is shard count (8 -> 16), not recipe changes.
