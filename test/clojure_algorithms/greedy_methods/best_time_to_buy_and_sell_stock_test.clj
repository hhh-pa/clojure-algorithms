(ns clojure-algorithms.greedy-methods.best-time-to-buy-and-sell-stock-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.best-time-to-buy-and-sell-stock :refer [max-profit]]))

(deftest test-max-profit
  (is (= 5 (max-profit [7 1 5 3 6 4])))
  (is (= 0 (max-profit [7 6 4 3 1])))
  (is (= 0 (max-profit [])))
  (is (= 0 (max-profit [5])))
  (is (= 4 (max-profit [1 2 3 4 5]))))
