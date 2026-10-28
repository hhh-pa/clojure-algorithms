(ns clojure-algorithms.greedy-methods.fractional-knapsack-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.fractional-knapsack :refer [frac-knapsack]]))

(deftest test-frac-knapsack
  (is (= 240N (bigint (frac-knapsack [60 100 120] [10 20 30] 50 3))))
  (is (= 105 (frac-knapsack [10 40 30 50] [5 4 6 3] 10 4)))
  (is (= 95 (frac-knapsack [10 40 30 50] [5 4 6 3] 8 4)))
  (is (= 0 (frac-knapsack [10 40 30 50] [5 4 6 3] 0 4)))
  (is (= 130 (frac-knapsack [10 40 30 50] [5 4 6 3] 800 4))))
