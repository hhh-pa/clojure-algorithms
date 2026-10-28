(ns clojure-algorithms.bit-manipulation.bitwise-addition-recursive
  "Sum of two non-negative integers using only bitwise operators.
  See https://en.wikipedia.org/wiki/Binary_number")

(defn bitwise-addition-recursive
  "Returns (+ number other-number) computed recursively with xor/and/shift.
  Both arguments must be non-negative integers."
  [number other-number]
  (when (or (not (integer? number)) (not (integer? other-number)))
    (throw (ex-info "Both arguments MUST be integers!" {})))
  (when (or (neg? number) (neg? other-number))
    (throw (ex-info "Both arguments MUST be non-negative!" {})))
  (loop [n number m other-number]
    (let [s (bit-xor n m)
          carry (bit-and n m)]
      (if (zero? carry)
        s
        (recur s (bit-shift-left carry 1))))))
