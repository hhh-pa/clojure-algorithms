(ns clojure-algorithms.maths.sieve-of-eratosthenes-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sieve-of-eratosthenes :as sut]))

(deftest test-fn-prime-sieve-examples
  (is (approx [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47] (sut/prime-sieve 50)))
  (is (approx [2 3 5 7 11 13 17 19 23] (sut/prime-sieve 25)))
  (is (approx [2 3 5 7] (sut/prime-sieve 10)))
  (is (approx [2 3 5 7] (sut/prime-sieve 9)))
  (is (approx [2] (sut/prime-sieve 2)))
  (is (approx [] (sut/prime-sieve 1))))
