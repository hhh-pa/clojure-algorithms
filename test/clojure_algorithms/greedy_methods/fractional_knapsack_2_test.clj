(ns clojure-algorithms.greedy-methods.fractional-knapsack-2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.fractional-knapsack-2 :refer [fractional-knapsack]]))

(deftest test-fractional-knapsack
  (let [value [1 3 5 7 9]
        weight [0.9 0.7 0.5 0.3 0.1]]
    (is (= [25 [1 1 1 1 1]] (fractional-knapsack value weight 5)))
    (is (= [25 [1 1 1 1 1]] (fractional-knapsack value weight 15)))
    (is (= [16 [1 1 1 1]] (fractional-knapsack [1 3 5 7] weight 30))))
  (is (= [0 []] (fractional-knapsack [] [] 30))))
