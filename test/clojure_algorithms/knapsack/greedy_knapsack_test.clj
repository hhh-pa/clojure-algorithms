(ns clojure-algorithms.knapsack.greedy-knapsack-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.knapsack.greedy-knapsack :refer [calc-profit]]))

(deftest test-calc-profit
  (is (= 6 (calc-profit [1 2 3] [3 4 5] 15)))
  (is (= 27 (calc-profit [10 9 8] [3 4 5] 25)))
  (is (thrown? clojure.lang.ExceptionInfo (calc-profit [1 2] [1] 10)))
  (is (thrown? clojure.lang.ExceptionInfo (calc-profit [1] [1] 0)))
  (is (thrown? clojure.lang.ExceptionInfo (calc-profit [-1] [1] 10)))
  (is (thrown? clojure.lang.ExceptionInfo (calc-profit [1] [-1] 10))))
