(ns clojure-algorithms.boolean-algebra.or-gate
  "OR gate.")

(defn or-gate
  "Returns the OR of two binary inputs."
  [input-1 input-2]
  (if (or (= 1 input-1) (= 1 input-2)) 1 0))
