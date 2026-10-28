(ns clojure-algorithms.maths.liouville-lambda
  "== Liouville Lambda Function ==
The Liouville Lambda function, denoted by λ(n):
λ(n) is 1 if n is the product of an even number of prime numbers,
and -1 if it is the product of an odd number of primes.

https://en.wikipedia.org/wiki/Liouville_function"
  (:require [clojure-algorithms.maths.prime-factors :as prime-factors]))

(defn liouville-lambda
  "Return 1 when number has an even number of prime factors, -1 otherwise.
  Throws :TypeError for non-integers and :ValueError for integers below 1."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (when (< number 1)
    (throw (ex-info "Input must be a positive integer" {:type :ValueError})))
  (if (odd? (count (prime-factors/prime-factors number)))
    -1
    1))
