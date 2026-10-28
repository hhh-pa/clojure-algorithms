(ns clojure-algorithms.data-structures.arrays.merge-intervals-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.merge-intervals
             :refer [merge-intervals]]))

(deftest test-merge-intervals
  (is (= [[1 6] [8 10] [15 18]]
         (merge-intervals [[1 3] [2 6] [8 10] [15 18]])))
  (is (= [[1 5]] (merge-intervals [[1 4] [4 5]])))
  (is (= [[1 4] [6 8]] (merge-intervals [[6 8] [1 3] [2 4]])))
  (is (= [] (merge-intervals [])))
  (is (= [[1 4]] (merge-intervals [[1 4]]))))
