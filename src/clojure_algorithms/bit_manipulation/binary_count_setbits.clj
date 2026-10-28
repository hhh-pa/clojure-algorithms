(ns clojure-algorithms.bit-manipulation.binary-count-setbits
  "Count the number of 1 bits in the binary representation of an integer.")

(defn binary-count-setbits
  "Returns the number of set bits in non-negative integer `a`."
  [a]
  (when (float? a)
    (throw (ex-info "Input value must be a 'int' type" {})))
  (when (neg? a)
    (throw (ex-info "Input value must be a positive integer" {})))
  (Long/bitCount (long a)))
