(ns clojure-algorithms.bit-manipulation.is-power-of-two
  "Power-of-two test via bit manipulation: a power of two n has the form
  0..100..00 while n-1 is 0..011..11, so (n & (n-1)) is 0.")

(defn is-power-of-two
  "Returns true when non-negative integer `number` is a power of two.
  Note: like the Python reference, 0 counts as a power of two."
  [number]
  (when (neg? number)
    (throw (ex-info "number must not be negative" {})))
  (let [n (biginteger number)]
    (.equals (.and n (.subtract n java.math.BigInteger/ONE))
             java.math.BigInteger/ZERO)))
