(ns clojure-algorithms.backtracking.combination-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.combination-sum :refer [combination-sum]]))

(deftest combination-sum-test
  (testing "doctest examples"
    (is (= [[2 2 2 2] [2 3 3] [3 5]] (combination-sum [2 3 5] 8)))
    (is (= [[2 2 3] [7]] (combination-sum [2 3 6 7] 7))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (combination-sum [-8 2.3 0] 1)))
    (is (thrown? clojure.lang.ExceptionInfo (combination-sum [] 1))))
  (testing "no solution yields empty list"
    (is (= [] (combination-sum [5 6] 3)))))
