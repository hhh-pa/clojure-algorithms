(ns clojure-algorithms.boolean-algebra.nand-gate
  "NAND gate: 0 only when both inputs are 1.")

(defn nand-gate
  "Returns the NAND of two binary inputs."
  [input-1 input-2]
  (if (and (= 1 input-1) (= 1 input-2)) 0 1))
