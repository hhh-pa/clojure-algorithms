(ns clojure-algorithms.data-structures.arrays.set-matrix-zeroes-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.set-matrix-zeroes
             :refer [set-matrix-zeroes]]))

(deftest test-set-matrix-zeroes
  (is (= [[1 0 1] [0 0 0] [1 0 1]]
         (set-matrix-zeroes [[1 1 1] [1 0 1] [1 1 1]])))
  (is (= [[0 0 0 0] [0 4 5 0] [0 3 1 0]]
         (set-matrix-zeroes [[0 1 2 0] [3 4 5 2] [1 3 1 5]])))
  (is (= [[1 2] [3 4]] (set-matrix-zeroes [[1 2] [3 4]])))
  (is (= [[0]] (set-matrix-zeroes [[0]]))))
