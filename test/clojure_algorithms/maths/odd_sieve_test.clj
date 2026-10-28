(ns clojure-algorithms.maths.odd-sieve-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.odd-sieve :as sut]))

(deftest test-fn-odd-sieve-examples
  (is (approx [] (sut/odd-sieve 2)))
  (is (approx [2] (sut/odd-sieve 3)))
  (is (approx [2 3 5 7] (sut/odd-sieve 10)))
  (is (approx [2 3 5 7 11 13 17 19] (sut/odd-sieve 20))))
