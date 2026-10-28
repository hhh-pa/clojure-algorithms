(ns clojure-algorithms.maths.primelib-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.primelib :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/is-prime))
  (is (some? #'sut/sieve-er))
  (is (some? #'sut/get-prime-numbers))
  (is (some? #'sut/prime-factorization))
  (is (some? #'sut/greatest-prime-factor))
  (is (some? #'sut/smallest-prime-factor))
  (is (some? #'sut/is-even))
  (is (some? #'sut/is-odd))
  (is (some? #'sut/goldbach))
  (is (some? #'sut/kg-v))
  (is (some? #'sut/get-prime))
  (is (some? #'sut/get-primes-between))
  (is (some? #'sut/get-divisors))
  (is (some? #'sut/is-perfect-number))
  (is (some? #'sut/simplify-fraction))
  (is (some? #'sut/factorial))
  (is (some? #'sut/fib)))
