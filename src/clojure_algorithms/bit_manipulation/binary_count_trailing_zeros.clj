(ns clojure-algorithms.bit-manipulation.binary-count-trailing-zeros
  "Count trailing zeros in the binary representation of a positive integer.")

(defn binary-count-trailing-zeros
  "Returns the number of trailing zero bits in positive integer `a`."
  [a]
  (when (not (integer? a))
    (throw (ex-info "Input value must be an integer" {})))
  (when (zero? a)
    (throw (ex-info "Trailing zeros for 0 are undefined" {})))
  (when (neg? a)
    (throw (ex-info "Input value must be a positive integer" {})))
  (Long/numberOfTrailingZeros (long a)))
