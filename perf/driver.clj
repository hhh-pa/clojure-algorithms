;; Per-namespace benchmark driver for the perf-callgrind sweep (issue #1).
;; Same driver on both legs (identical workload):
;;   JVM:    java -cp '<jars>:src:test' clojure.main perf/driver.clj <ns>
;;   native: clojure-rs run perf/driver.clj <ns> src test
;; The optional load-path args exist for the native leg only (the JVM
;; leg resolves via -cp). Prints one SUMMARY line; the sweep gates on
;; it (missing SUMMARY or :fail/:error > 0 fails the case). No
;; System/exit (unemulated natively) — the gate reads stdout.
(when-let [lp (seq (rest *command-line-args*))]
  ;; Resolved dynamically: the JVM leg never passes load-path
  ;; args, and JVM Clojure has no `*load-path*` var at all (a
  ;; static `#'` reference would fail the whole script there).
  (when-let [v (resolve 'clojure.core/*load-path*)]
    (alter-var-root v (constantly (vec lp)))))
(require (symbol (first *command-line-args*)))
(let [sum (clojure.test/run-tests (symbol (first *command-line-args*)))]
  (println "SUMMARY" (select-keys sum [:test :pass :fail :error])))
