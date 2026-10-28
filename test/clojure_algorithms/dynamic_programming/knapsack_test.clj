(ns clojure-algorithms.dynamic-programming.knapsack-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.knapsack :as k]))

(deftest knapsack-test
  (testing "optimal values"
    (is (= 8 (first (k/knapsack 6 [4 3 2 3] [3 2 4 4] 4))))
    (is (= 8 (k/mf-knapsack 4 [4 3 2 3] [3 2 4 4] 6)))
    (is (= 142 (first (k/knapsack 10 [1 3 5 2] [10 20 100 22] 4)))))
  (testing "example solutions"
    (is (= [142 #{2 3 4}]
           (k/knapsack-with-example-solution 10 [1 3 5 2] [10 20 100 22])))
    (is (= [8 #{3 4}]
           (k/knapsack-with-example-solution 6 [4 3 2 3] [3 2 4 4]))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException
                 (k/knapsack-with-example-solution 6 [4 3 2 3] [3 2 4])))
    (is (thrown? IllegalArgumentException
                 (k/knapsack-with-example-solution 6 [4 3 2 1.5] [3 2 4 4])))))
