(ns clojure-algorithms.project-euler.problem-097.sol1
  "Project Euler 97: large non-Mersenne prime.

  In 2004 there was found a massive non-Mersenne prime which contains
  2,357,207 digits: 28433 * 2^7830457 + 1. Find the last ten digits of
  this prime number."
  (:import [java.math BigInteger]))

(defn solution
  "Returns the last n digits of 28433 * 2^7830457 + 1 as a string."
  ([] (solution 10))
  ([n]
   (when-not (and (integer? n) (>= n 0))
     (throw (IllegalArgumentException. "Invalid input")))
   (let [modulus (.pow BigInteger/TEN (int n))
         number (.add (.multiply (BigInteger/valueOf 28433)
                                 (.modPow BigInteger/TWO
                                          (BigInteger/valueOf 7830457)
                                          modulus))
                      BigInteger/ONE)]
     (str (.mod number modulus)))))
