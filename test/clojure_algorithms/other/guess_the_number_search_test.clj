(ns clojure-algorithms.other.guess-the-number-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.guess-the-number-search :as g]))

(deftest get-avg-test
  (is (= 12 (g/get-avg 10 15)))
  (is (= 160 (g/get-avg 20 300))))

(deftest guess-sequence-test
  (testing "doctest traces"
    (is (= [505 257 133 71 40 25 17] (g/guess-sequence 10 1000 17)))
    (is (= [0 5000 2500 1250 625 312 156 78 39 19 9 4 6 7]
           (g/guess-sequence -10000 10000 7))))
  (testing "guess outside range throws"
    (is (thrown? IllegalArgumentException (g/guess-sequence 10 1000 5))))
  (testing "lower above higher throws"
    (is (thrown? IllegalArgumentException (g/guess-sequence 10000 100 5))))
  (testing "non-integers throw"
    (is (thrown? AssertionError (g/guess-sequence 10 1000 "a")))))
