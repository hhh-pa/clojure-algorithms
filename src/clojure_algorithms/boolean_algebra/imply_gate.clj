(ns clojure-algorithms.boolean-algebra.imply-gate
  "IMPLY gate: (not a) or b.")

(defn imply-gate
  "Returns the IMPLY of two binary inputs."
  [input-1 input-2]
  (if (or (zero? input-1) (= 1 input-2)) 1 0))

(defn recursive-imply-list
  "Applies IMPLY left to right over a list of at least two bits."
  [input-list]
  (when (< (count input-list) 2)
    (throw (IllegalArgumentException.
            "Input list must contain at least two elements")))
  (reduce imply-gate input-list))
