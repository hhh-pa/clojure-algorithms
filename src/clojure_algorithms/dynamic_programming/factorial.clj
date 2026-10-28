(ns clojure-algorithms.dynamic-programming.factorial
  "Factorial with memoization.
  Ported from TheAlgorithms/Python dynamic_programming/factorial.py.")

(def ^:private memo (atom {0 1 1 1}))

(defn factorial
  "n! for non-negative integer `n`."
  [n]
  (when (neg? n)
    (throw (IllegalArgumentException. "Number should not be negative.")))
  (if (contains? @memo n)
    (@memo n)
    (let [result (* n (factorial (dec n)))]
      (swap! memo assoc n result)
      result)))
