(ns clojure-algorithms.boolean-algebra.not-gate
  "NOT gate.")

(defn not-gate
  "Returns the NOT of a binary input."
  [input-1]
  (if (zero? input-1) 1 0))
