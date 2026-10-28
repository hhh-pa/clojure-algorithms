# Contributing

Thanks for helping port [TheAlgorithms/Python](https://github.com/TheAlgorithms/Python)
to Clojure!

## Ground rules

1. **One upstream file → one Clojure namespace.** Mirror the upstream category:
   `sorts/bubble_sort.py` becomes `src/clojure_algorithms/sorts/bubble_sort.clj`
   in namespace `clojure-algorithms.sorts.bubble-sort`.
2. **Idiomatic Clojure.** Prefer pure functions, immutable data, and `clojure.test`
   tests over transliterated Python. Keep public function names kebab-case.
3. **Every algorithm ships with tests.** Add a matching test namespace under
   `test/clojure_algorithms/...` covering the upstream doctests/examples plus
   edge cases.
4. **Cite the source.** Each namespace docstring must link the upstream file it
   ports, e.g. `https://github.com/TheAlgorithms/Python/blob/master/sorts/bubble_sort.py`.
5. **Format before pushing.** Run `clojure -M:cljfmt`; CI fails on unformatted code.

## Workflow

```bash
clojure -P            # fetch deps
clojure -M:test       # run tests
clojure -M:cljfmt     # check formatting
```

## What not to port

- Upstream test harnesses, scripts, and docs (`scripts/`, `docs/`, bare `tests/`
  helpers) — only the algorithm modules and their doctests/examples.
- Anything requiring Python-only native extensions without a JVM equivalent;
  note it in your PR instead.

## License

Contributions are accepted under the MIT License (see [LICENSE](LICENSE)).
