(ns clojure-algorithms.matrix.matrix-operation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.matrix-operation :as sut]))

(deftest test-fn-add-examples
  (is (approx [[3 5] [7 9]] (sut/add [[1 2] [3 4]] [[2 3] [4 5]])))
  (is (approx [[3.2 5.4] [7 9]] (sut/add [[1.2 2.4] [3 4]] [[2 3] [4 5]])))
  (is (approx [[7 14] [12 16]] (sut/add [[1 2] [4 5]] [[3 7] [3 4]] [[3 5] [5 7]])))
  (is (thrown? Exception (sut/add [3] [4 5]))))

(deftest test-fn-subtract-examples
  (is (approx [[-1 -1] [-1 -1]] (sut/subtract [[1 2] [3 4]] [[2 3] [4 5]])))
  (is (approx [[-1 -0.5] [-1 -1.5]] (sut/subtract [[1 2.5] [3 4]] [[2 3] [4 5.5]])))
  (is (thrown? Exception (sut/subtract [3] [4 5]))))

(deftest test-fn-scalar-multiply-examples
  (is (approx [[5 10] [15 20]] (sut/scalar-multiply [[1 2] [3 4]] 5)))
  (is (approx [[7.0 11.5] [15 20]] (sut/scalar-multiply [[1.4 2.3] [3 4]] 5))))

(deftest test-fn-multiply-examples
  (is (approx [[19 15] [43 35]] (sut/multiply [[1 2] [3 4]] [[5 5] [7 5]])))
  (is (approx [[22.5 17.5] [46.5 37.5]] (sut/multiply [[1 2.5] [3 4.5]] [[5 5] [7 5]])))
  (is (approx [[20]] (sut/multiply [[1 2 3]] [[2] [3] [4]]))))

(deftest test-fn-identity-examples
  (is (approx [[1 0 0] [0 1 0] [0 0 1]] (sut/identity 3))))

(deftest test-fn-transpose-examples
  (is (thrown? Exception (sut/transpose [1 [2 3]]))))

(deftest test-fn-minor-examples
  (is (approx [[1]] (sut/minor [[1 2] [3 4]] 1 1))))

(deftest test-fn-determinant-examples
  (is (approx -2 (sut/determinant [[1 2] [3 4]])))
  (is (approx -1.5 (sut/determinant [[1.5 2.5] [3 4]]))))

(deftest test-fn-permanent-examples
  (is (approx 10 (sut/permanent [[1 2] [3 4]])))
  (is (approx 13.5 (sut/permanent [[1.5 2.5] [3 4]]))))

(deftest test-fn-inverse-examples
  (is (approx [[-2.0 1.0] [1.5 -0.5]] (sut/inverse [[1 2] [3 4]])))
  (is (nil? (sut/inverse [[1 1] [1 1]]))))
