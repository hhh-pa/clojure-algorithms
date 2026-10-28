(ns clojure-algorithms.boolean-algebra.xnor-gate
  "XNOR gate: 1 when both inputs are equal.")

(defn xnor-gate
  "Returns the XNOR of two binary inputs."
  [input-1 input-2]
  (if (= input-1 input-2) 1 0))
