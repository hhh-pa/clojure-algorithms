(ns clojure-algorithms.maths.matrix-exponentiation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.matrix-exponentiation :as sut]))

(deftest test-fn-fibonacci-with-matrix-exponentiation-examples
  (is (approx 5 (sut/fibonacci-with-matrix-exponentiation 1 5 6)))
  (is (approx 11 (sut/fibonacci-with-matrix-exponentiation 2 10 11)))
  (is (approx 144 (sut/fibonacci-with-matrix-exponentiation 13 0 1)))
  (is (approx 411 (sut/fibonacci-with-matrix-exponentiation 10 5 9)))
  (is (approx 89 (sut/fibonacci-with-matrix-exponentiation 9 2 3))))

(deftest test-fn-simple-fibonacci-examples
  (is (approx 5 (sut/simple-fibonacci 1 5 6)))
  (is (approx 11 (sut/simple-fibonacci 2 10 11)))
  (is (approx 144 (sut/simple-fibonacci 13 0 1)))
  (is (approx 411 (sut/simple-fibonacci 10 5 9)))
  (is (approx 89 (sut/simple-fibonacci 9 2 3))))
