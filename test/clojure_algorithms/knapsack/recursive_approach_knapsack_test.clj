(ns clojure-algorithms.knapsack.recursive-approach-knapsack-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.knapsack.recursive-approach-knapsack :refer [knapsack]]))

(deftest test-knapsack
  (is (= 13 (knapsack [1 2 4 5] [5 4 8 6] 4 5 0)))
  (is (= 27 (knapsack [3 4 5] [10 9 8] 3 25 0)))
  (is (= 0 (knapsack [1] [5] 1 0 0))))
