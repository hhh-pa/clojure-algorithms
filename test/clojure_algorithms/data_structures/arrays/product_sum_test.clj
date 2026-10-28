(ns clojure-algorithms.data-structures.arrays.product-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.product-sum
             :refer [product-sum product-sum-array product-sum-iterative]]))

(deftest test-product-sum
  (is (= 6 (product-sum [1 2 3] 1)))
  (is (= 8 (product-sum [-1 2 [-3 4]] 2)))
  (is (= -6 (product-sum [1 2 3] -1)))
  (is (= 0 (product-sum [1 2 3] 0)))
  (is (= 42 (product-sum [1 2 3] 7)))
  (is (= 0 (product-sum [1 -1] 1)))
  (is (= -1 (product-sum [1 -2] 1)))
  (is (= 1.5 (product-sum [-3.5 [1 [0.5]]] 1))))

(deftest test-product-sum-array
  (is (= 6 (product-sum-array [1 2 3])))
  (is (= 11 (product-sum-array [1 [2 3]])))
  (is (= 47 (product-sum-array [1 [2 [3 4]]])))
  (is (= 0 (product-sum-array [0])))
  (is (= 1.5 (product-sum-array [-3.5 [1 [0.5]]])))
  (is (= -1 (product-sum-array [1 -2]))))

(deftest test-product-sum-iterative
  (doseq [a [[1 2 3] [1 [2 3]] [1 [2 [3 4]]] [0]
             [5 2 [-7 1] 3 [6 [-13 8] 4]] [-3.5 [1 [0.5]]]]]
    (is (= (product-sum-array a) (product-sum-iterative a))))
  (is (= -12 (product-sum-iterative [5 2 [-7 1] 3 [6 [-13 8] 4]]))))
