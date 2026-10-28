(ns clojure-algorithms.boolean-algebra.nimply-gate
  "NIMPLY gate: a and (not b).")

(defn nimply-gate
  "Returns the NIMPLY of two binary inputs."
  [input-1 input-2]
  (if (and (= 1 input-1) (zero? input-2)) 1 0))
