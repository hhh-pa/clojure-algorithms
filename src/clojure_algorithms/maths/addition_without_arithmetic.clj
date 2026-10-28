(ns clojure-algorithms.maths.addition-without-arithmetic
  "Illustrate how to add the integer without arithmetic operation.
Author: suraj Kumar
https://en.wikipedia.org/wiki/Bitwise_operation")

(defn add
  "Add two integers using bitwise operations instead of arithmetic operators:
  XOR (bit-xor) adds bits without carry, AND (bit-and) computes the carry
  bits, and left shift moves the carry to the correct position.
  Works for arbitrary precision integers, including negatives."
  [first-v second-v]
  (loop [a (biginteger first-v)
         b (biginteger second-v)]
    (if (.equals ^java.math.BigInteger b java.math.BigInteger/ZERO)
      (try
        (.longValueExact ^java.math.BigInteger a)
        (catch ArithmeticException _
          (bigint a)))
      (recur (.xor ^java.math.BigInteger a b)
             (.shiftLeft (.and ^java.math.BigInteger a b) 1)))))
