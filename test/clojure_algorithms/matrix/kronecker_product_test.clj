(ns clojure-algorithms.matrix.kronecker-product-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.kronecker-product :as sut]))

(deftest test-fn-is-2d-examples
  (is (= true (boolean (sut/is-2d []))))
  (is (= false (boolean (sut/is-2d [1 2]))))
  (is (= true (boolean (sut/is-2d [[1 2] [3 4]])))))

(deftest test-fn-kronecker-product-examples
  (is (approx [[5 6 10 12] [7 8 14 16]] (sut/kronecker-product [[1 2]] [[5 6] [7 8]])))
  (is (approx [[5 6 10 12] [7 8 14 16] [20 24 25 30] [28 32 35 40]] (sut/kronecker-product [[1 2] [4 5]] [[5 6] [7 8]])))
  (is (thrown? Exception (sut/kronecker-product [1 2] [[5 6] [7 8]]))))
