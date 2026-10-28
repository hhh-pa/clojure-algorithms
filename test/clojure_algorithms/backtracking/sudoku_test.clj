(ns clojure-algorithms.backtracking.sudoku-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.sudoku
             :refer [safe? find-empty-location sudoku initial-grid no-solution]]))

(deftest sudoku-test
  (testing "doctest solution"
    (is (= [[3 1 6 5 7 8 4 9 2]
            [5 2 9 1 3 4 7 6 8]
            [4 8 7 6 2 9 5 3 1]
            [2 6 3 4 1 5 9 8 7]
            [9 7 4 8 6 3 1 2 5]
            [8 5 1 7 9 2 6 4 3]
            [1 3 8 9 4 7 2 5 6]
            [6 9 2 3 5 1 8 7 4]
            [7 4 5 2 8 6 3 1 9]]
           (sudoku initial-grid))))
  (testing "unsolvable grid returns nil"
    (is (nil? (sudoku no-solution))))
  (testing "solution satisfies Sudoku constraints"
    (let [solved (sudoku initial-grid)]
      (doseq [row solved]
        (is (= (set (range 1 10)) (set row))))
      (doseq [c (range 9)]
        (is (= (set (range 1 10)) (set (map #(nth % c) solved))))))))

(deftest safe-test
  (is (true? (safe? initial-grid 0 1 1)))
  (is (false? (safe? initial-grid 0 1 3))))

(deftest find-empty-location-test
  (is (= [0 1] (find-empty-location initial-grid)))
  (is (nil? (find-empty-location (sudoku initial-grid)))))
