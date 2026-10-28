(ns clojure-algorithms.backtracking.minimax-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.minimax :refer [minimax optimal-value]]))

(defn- log2 [n] (/ (Math/log n) (Math/log 2)))

(deftest minimax-test
  (testing "doctest examples"
    (let [scores [90 23 6 33 21 65 123 34423]]
      (is (= 65 (minimax 0 0 true scores (log2 (count scores))))))
    (let [scores [3 5 2 9 12 5 23 23]]
      (is (= 12 (minimax 0 0 true scores (log2 (count scores)))))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (minimax -1 0 true [1 2] 1)))
    (is (thrown? clojure.lang.ExceptionInfo (minimax 0 0 true [] 2)))
    (is (thrown? clojure.lang.ExceptionInfo (minimax 0 0 true [1 2 3] 2))))
  (testing "optimal-value helper"
    (is (= 65 (optimal-value [90 23 6 33 21 65 123 34423])))))
