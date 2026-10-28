(ns clojure-algorithms.bit-manipulation.index-of-rightmost-set-bit
  "Zero-based index of the rightmost set bit of a non-negative integer.
  Ref: https://www.geeksforgeeks.org/position-of-rightmost-set-bit/")

(defn get-index-of-rightmost-set-bit
  "Returns the zero-based index of the first set bit from the right,
  or -1 when `number` has no set bit. `number` must be non-negative."
  [number]
  (when (or (not (integer? number)) (neg? number))
    (throw (ex-info "Input must be a non-negative integer" {})))
  (if (zero? number)
    -1
    (Long/numberOfTrailingZeros (long number))))
