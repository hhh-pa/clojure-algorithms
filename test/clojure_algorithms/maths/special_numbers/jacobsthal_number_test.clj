(ns clojure-algorithms.maths.special-numbers.jacobsthal-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.jacobsthal-number :as sut]))

(deftest test-fn-jacobsthal-number-examples
  (is (approx 0 (sut/jacobsthal-number 0)))
  (is (approx 1 (sut/jacobsthal-number 1)))
  (is (approx 11 (sut/jacobsthal-number 5)))
  (is (approx 341 (sut/jacobsthal-number 10)))
  (is (approx 6148914691236517205 (sut/jacobsthal-number 64)))
  (is (thrown? Exception (sut/jacobsthal-number -1)))
  (is (thrown? Exception (sut/jacobsthal-number 2.5)))
  (is (thrown? Exception (sut/jacobsthal-number "3"))))

(deftest test-fn-jacobsthal-sequence-examples
  (is (approx [] (sut/jacobsthal-sequence 0)))
  (is (approx [0] (sut/jacobsthal-sequence 1)))
  (is (approx [0 1 1 3 5 11 21 43 85 171 341] (sut/jacobsthal-sequence 11)))
  (is (thrown? Exception (sut/jacobsthal-sequence -3)))
  (is (thrown? Exception (sut/jacobsthal-sequence 4.0))))
