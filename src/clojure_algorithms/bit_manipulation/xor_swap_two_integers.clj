(ns clojure-algorithms.bit-manipulation.xor-swap-two-integers
  "Swap two integers with XOR, without a temporary variable.
  See https://en.wikipedia.org/wiki/Bitwise_operation#XOR")

(defn xor-swap
  "Returns `[b a]`: `a` and `b` swapped via three XOR steps."
  [a b]
  (let [a (bit-xor a b)
        b (bit-xor a b)
        a (bit-xor a b)]
    [a b]))
