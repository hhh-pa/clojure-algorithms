(ns clojure-algorithms.bit-manipulation.numbers-different-signs
  "Detect opposite signs with XOR: (num1 ^ num2) < 0 means the sign bits differ.")

(defn different-signs
  "Returns true when `num1` and `num2` have opposite signs."
  [num1 num2]
  ;; BigInteger interop: Clojure's bit-xor rejects BigInt operands.
  (neg? (.signum (.xor (biginteger num1) (biginteger num2)))))
