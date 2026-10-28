(ns clojure-algorithms.maths.primelib-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.primelib :as sut]))

(deftest test-fn-is-prime-examples
  (is (= true (sut/is-prime 3)))
  (is (= false (sut/is-prime 10)))
  (is (= true (sut/is-prime 97)))
  (is (= false (sut/is-prime 9991)))
  (is (thrown? AssertionError (sut/is-prime -1)))
  (is (thrown? AssertionError (sut/is-prime "test"))))

(deftest test-fn-sieve-er-examples
  (is (= [2 3 5 7] (sut/sieve-er 8)))
  (is (thrown? AssertionError (sut/sieve-er -1)))
  (is (thrown? AssertionError (sut/sieve-er "test"))))

(deftest test-fn-get-prime-numbers-examples
  (is (= [2 3 5 7] (sut/get-prime-numbers 8)))
  (is (thrown? AssertionError (sut/get-prime-numbers -1)))
  (is (thrown? AssertionError (sut/get-prime-numbers "test"))))

(deftest test-fn-prime-factorization-examples
  (is (= [0] (sut/prime-factorization 0)))
  (is (= [2 2 2] (sut/prime-factorization 8)))
  (is (= [7 41] (sut/prime-factorization 287)))
  (is (thrown? AssertionError (sut/prime-factorization -1)))
  (is (thrown? AssertionError (sut/prime-factorization "test"))))

(deftest test-fn-greatest-prime-factor-examples
  (is (= 0 (sut/greatest-prime-factor 0)))
  (is (= 2 (sut/greatest-prime-factor 8)))
  (is (= 41 (sut/greatest-prime-factor 287)))
  (is (thrown? AssertionError (sut/greatest-prime-factor -1)))
  (is (thrown? AssertionError (sut/greatest-prime-factor "test"))))

(deftest test-fn-smallest-prime-factor-examples
  (is (= 0 (sut/smallest-prime-factor 0)))
  (is (= 2 (sut/smallest-prime-factor 8)))
  (is (= 7 (sut/smallest-prime-factor 287)))
  (is (thrown? AssertionError (sut/smallest-prime-factor -1)))
  (is (thrown? AssertionError (sut/smallest-prime-factor "test"))))

(deftest test-fn-is-even-examples
  (is (= true (sut/is-even 0)))
  (is (= true (sut/is-even 8)))
  (is (= false (sut/is-even 287)))
  (is (= false (sut/is-even -1)))
  (is (thrown? AssertionError (sut/is-even "test"))))

(deftest test-fn-is-odd-examples
  (is (= false (sut/is-odd 0)))
  (is (= false (sut/is-odd 8)))
  (is (= true (sut/is-odd 287)))
  (is (= true (sut/is-odd -1)))
  (is (thrown? AssertionError (sut/is-odd "test"))))

(deftest test-fn-goldbach-examples
  (is (= [3 5] (sut/goldbach 8)))
  (is (= [3 821] (sut/goldbach 824)))
  (is (thrown? AssertionError (sut/goldbach 0)))
  (is (thrown? AssertionError (sut/goldbach -1)))
  (is (thrown? AssertionError (sut/goldbach "test"))))

(deftest test-fn-kg-v-examples
  (is (= 40 (sut/kg-v 8 10)))
  (is (= 55208 (sut/kg-v 824 67)))
  (is (= 10 (sut/kg-v 1 10)))
  (is (thrown? Exception (sut/kg-v 0)))
  (is (thrown? AssertionError (sut/kg-v 10 -1)))
  (is (thrown? AssertionError (sut/kg-v "test" "test2"))))

(deftest test-fn-get-prime-examples
  (is (= 2 (sut/get-prime 0)))
  (is (= 23 (sut/get-prime 8)))
  (is (= 6337 (sut/get-prime 824)))
  (is (thrown? AssertionError (sut/get-prime -1)))
  (is (thrown? AssertionError (sut/get-prime "test"))))

(deftest test-fn-get-primes-between-examples
  (is (= [5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61]
         (sut/get-primes-between 3 67)))
  (is (thrown? Exception (sut/get-primes-between 0)))
  (is (thrown? AssertionError (sut/get-primes-between 0 1)))
  (is (thrown? AssertionError (sut/get-primes-between -1 3)))
  (is (thrown? AssertionError (sut/get-primes-between "test" "test"))))

(deftest test-fn-get-divisors-examples
  (is (= [1 2 4 8] (sut/get-divisors 8)))
  (is (= [1 2 4 8 103 206 412 824] (sut/get-divisors 824)))
  (is (thrown? AssertionError (sut/get-divisors -1)))
  (is (thrown? AssertionError (sut/get-divisors "test"))))

(deftest test-fn-is-perfect-number-examples
  (is (= true (sut/is-perfect-number 28)))
  (is (= false (sut/is-perfect-number 824)))
  (is (thrown? AssertionError (sut/is-perfect-number -1)))
  (is (thrown? AssertionError (sut/is-perfect-number "test"))))

(deftest test-fn-simplify-fraction-examples
  (is (= [1 2] (sut/simplify-fraction 10 20)))
  (is (= [10 -1] (sut/simplify-fraction 10 -1)))
  (is (thrown? AssertionError (sut/simplify-fraction "test" "test"))))

(deftest test-fn-factorial-examples
  (is (= 1 (sut/factorial 0)))
  (is (= 2432902008176640000 (sut/factorial 20)))
  (is (thrown? AssertionError (sut/factorial -1)))
  (is (thrown? AssertionError (sut/factorial "test"))))

(deftest test-fn-fib-examples
  (is (= 1 (sut/fib 0)))
  (is (= 8 (sut/fib 5)))
  (is (= 10946 (sut/fib 20)))
  (is (= 354224848179261915075 (sut/fib 99)))
  (is (thrown? AssertionError (sut/fib -1)))
  (is (thrown? AssertionError (sut/fib "test"))))
