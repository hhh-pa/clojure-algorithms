(ns clojure-algorithms.backtracking.rat-in-maze-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.rat-in-maze :refer [solve-maze]]))

(deftest solve-maze-test
  (testing "5x5 maze with walls"
    (let [maze [[0 1 0 1 1]
                [0 0 0 0 0]
                [1 0 1 0 1]
                [0 0 1 0 0]
                [1 0 0 1 0]]]
      (is (= [[0 1 1 1 1]
              [0 0 0 0 1]
              [1 1 1 0 1]
              [1 1 1 0 0]
              [1 1 1 1 0]]
             (solve-maze maze 0 0 4 4)))))
  (testing "open 5x5 maze"
    (let [maze [[0 1 0 1 1]
                [0 0 0 0 0]
                [0 0 0 0 1]
                [0 0 0 0 0]
                [0 0 0 0 0]]]
      (is (= [[0 1 1 1 1]
              [0 1 1 1 1]
              [0 1 1 1 1]
              [0 1 1 1 1]
              [0 0 0 0 0]]
             (solve-maze maze 0 0 4 4)))))
  (testing "3x3 mazes"
    (is (= [[0 0 0] [1 1 0] [1 1 0]]
           (solve-maze [[0 0 0] [0 1 0] [1 0 0]] 0 0 2 2)))
    (is (= [[1 0 0] [1 1 0] [1 1 0]]
           (solve-maze [[1 0 0] [0 1 0] [1 0 0]] 0 1 2 2))))
  (testing "8x8 maze"
    (let [maze [[1 1 0 0 1 0 0 1]
                [1 0 1 0 0 1 1 1]
                [0 1 0 1 0 0 1 0]
                [1 1 1 0 0 1 0 1]
                [0 1 0 0 1 0 1 1]
                [0 0 0 1 1 1 0 1]
                [0 1 0 1 0 1 1 1]
                [1 1 0 0 0 0 0 1]]]
      (is (= [[1 1 0 0 1 1 1 1]
              [1 1 1 0 0 1 1 1]
              [1 1 1 1 0 1 1 1]
              [1 1 1 0 0 1 1 1]
              [1 1 0 0 1 1 1 1]
              [1 1 0 1 1 1 1 1]
              [1 1 0 1 1 1 1 1]
              [1 1 0 1 1 1 1 1]]
             (solve-maze maze 0 2 7 2)))))
  (testing "no solution raises"
    (is (thrown? clojure.lang.ExceptionInfo
                 (solve-maze [[1 0 0] [0 1 1] [1 0 1]] 0 1 2 2)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (solve-maze [[0 0] [1 1]] 0 0 1 1))))
  (testing "invalid coordinates raise"
    (is (thrown? clojure.lang.ExceptionInfo
                 (solve-maze [[0 1] [1 0]] 2 0 1 1)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (solve-maze [[1 0 0] [0 1 0] [1 0 0]] 0 1 3 2)))))
