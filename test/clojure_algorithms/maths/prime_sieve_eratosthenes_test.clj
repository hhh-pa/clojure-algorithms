(ns clojure-algorithms.maths.prime-sieve-eratosthenes-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.prime-sieve-eratosthenes :as sut]))

(deftest test-fn-prime-sieve-eratosthenes-examples
  (is (approx [2 3 5 7] (sut/prime-sieve-eratosthenes 10)))
  (is (approx [2 3 5 7 11 13 17 19] (sut/prime-sieve-eratosthenes 20)))
  (is (approx [2] (sut/prime-sieve-eratosthenes 2)))
  (is (approx [] (sut/prime-sieve-eratosthenes 1)))
  (is (thrown? Exception (sut/prime-sieve-eratosthenes -1))))
