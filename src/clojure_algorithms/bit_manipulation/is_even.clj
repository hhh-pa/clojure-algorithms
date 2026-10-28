(ns clojure-algorithms.bit-manipulation.is-even
  "Evenness tests via bit manipulation.")

(defn is-even
  "Returns true when integer `number` is even, using the lowest bit.
  Non-integers (including booleans) raise."
  [number]
  (when (or (not (integer? number)) (boolean? number))
    (throw (ex-info "input must be an integer" {})))
  (zero? (bit-and number 1)))

(defn is-even-using-shift-operator
  "Returns true when `number` is even: shifting right then left drops the
  last bit, so equality with the original implies evenness."
  [number]
  (= number (bit-shift-left (bit-shift-right number 1) 1)))
