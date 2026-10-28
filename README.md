# Clojure Algorithms

A Clojure port of [TheAlgorithms/Python](https://github.com/TheAlgorithms/Python) —
classic algorithms and data structures implemented in idiomatic Clojure.

> **Port notice:** this repository is a community port. Algorithm selection,
> category layout, and docstrings follow the upstream Python project; every
> implementation is rewritten in Clojure. Upstream is the reference for
> correctness and scope.

## Attribution

- Upstream project: [TheAlgorithms/Python](https://github.com/TheAlgorithms/Python)
  (MIT License, © TheAlgorithms and contributors), at commit
  [`6e03a48`](https://github.com/TheAlgorithms/Python/commit/6e03a48)
  (2026-10-19).
- This port keeps the upstream category structure so contributions can be
  compared side by side. See [CONTRIBUTING.md](CONTRIBUTING.md).

## Status

- **1,350** implementations, each with a matching `clojure.test` suite
  (**1,350** test namespaces).
- Full suite: **2,418 tests, 0 failures, 0 errors** (`clojure -M:test`;
  ≈13,800 assertions — the exact figure varies slightly run to run
  because one property test asserts per edge of a random graph).
- Deliberate deviations from upstream are marked `NOTE` in code and are
  limited to cases where upstream is broken (RSA keygen enforces `p ≠ q`,
  which upstream omits).
- Implementations that depend on Python-only libraries (NumPy, OpenCV,
  network APIs) are ported as pure-Clojure logic where possible, otherwise
  as documented stubs — see the namespace docstrings.

## Requirements

- Java 21+
- [Clojure CLI](https://clojure.org/guides/install_clojure) (`clojure`)

## Usage

```bash
# Download dependencies
clojure -P

# Run the test suite
clojure -M:test

# Check formatting
clojure -M:cljfmt

# Build a jar
clojure -T:build jar
```

## Structure

```
src/clojure_algorithms/
  core.clj            # placeholder entry point
  <category>/...     # one namespace tree per upstream category
test/clojure_algorithms/
  core_test.clj
  <category>/...     # matching test suites
resources/
  words.txt          # vendored from upstream (strings/anagrams)
  dictionary.txt     # vendored from upstream (strings/detecting-english)
```

## Coverage

| Category | Namespace | Impl | Tests |
|---|---|---:|---:|
| audio_filters | `audio-filters` | 4 | 4 |
| backtracking | `backtracking` | 22 | 22 |
| bit_manipulation | `bit-manipulation` | 31 | 31 |
| blockchain | `blockchain` | 6 | 6 |
| boolean_algebra | `boolean-algebra` | 12 | 12 |
| cellular_automata | `cellular-automata` | 7 | 7 |
| ciphers | `ciphers` | 53 | 53 |
| computer_vision | `computer-vision` | 11 | 11 |
| conversions | `conversions` | 40 | 40 |
| data_compression | `data-compression` | 9 | 9 |
| data_structures | `data-structures` | 129 | 129 |
| digital_image_processing | `digital-image-processing` | 20 | 20 |
| divide_and_conquer | `divide-and-conquer` | 13 | 13 |
| dynamic_programming | `dynamic-programming` | 52 | 52 |
| electronics | `electronics` | 23 | 23 |
| file_transfer | `file-transfer` | 2 | 2 |
| financial | `financial` | 17 | 17 |
| fractals | `fractals` | 8 | 8 |
| fuzzy_logic | `fuzzy-logic` | 2 | 2 |
| genetic_algorithm | `genetic-algorithm` | 2 | 2 |
| geodesy | `geodesy` | 3 | 3 |
| geometry | `geometry` | 10 | 10 |
| graphics | `graphics` | 6 | 6 |
| graphs | `graphs` | 66 | 66 |
| greedy_methods | `greedy-methods` | 12 | 12 |
| hashes | `hashes` | 14 | 14 |
| knapsack | `knapsack` | 3 | 3 |
| linear_algebra | `linear-algebra` | 14 | 14 |
| linear_programming | `linear-programming` | 1 | 1 |
| machine_learning | `machine-learning` | 45 | 45 |
| maths | `maths` | 211 | 211 |
| matrix | `matrix` | 24 | 24 |
| networking_flow | `networking-flow` | 4 | 4 |
| neural_network | `neural-network` | 24 | 24 |
| other | `other` | 31 | 31 |
| physics | `physics` | 50 | 50 |
| project_euler | `project-euler` | 181 | 181 |
| quantum | `quantum` | 1 | 1 |
| scheduling | `scheduling` | 10 | 10 |
| searches | `searches` | 17 | 17 |
| sorts | `sorts` | 59 | 59 |
| strings | `strings` | 66 | 66 |
| web_programming | `web-programming` | 35 | 35 |
| **Total** | | **1,350** | **1,350** |

## License

MIT — see [LICENSE](LICENSE).
