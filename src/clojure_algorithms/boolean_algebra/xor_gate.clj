(ns clojure-algorithms.boolean-algebra.xor-gate
  "XOR gate: 1 when exactly one input is 1.")

(defn xor-gate
  "Returns the XOR of two binary inputs."
  [input-1 input-2]
  (if (= input-1 input-2) 0 1))
