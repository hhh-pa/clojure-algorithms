(ns clojure-algorithms.knapsack.knapsack-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.knapsack.knapsack :refer [knapsack]]))

(deftest test-knapsack
  (let [cap 50 val [60 100 120] w [10 20 30] c (count val)]
    (is (= 220 (knapsack cap w val c)))
    (is (= 300 (knapsack cap w val c true))))
  (is (= 0 (knapsack 0 [10] [60] 1)))
  (is (= 0 (knapsack 50 [10] [60] 0))))
