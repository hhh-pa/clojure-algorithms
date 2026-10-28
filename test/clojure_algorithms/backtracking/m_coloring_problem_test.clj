(ns clojure-algorithms.backtracking.m-coloring-problem-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.m-coloring-problem
             :refer [safe? solve graph-coloring]]))

(deftest safe-test
  (is (false? (safe? 0 1 [[0 1] [1 0]] 2 [0 1])))
  (is (true? (safe? 0 2 [[0 1] [1 0]] 2 [0 1]))))

(deftest solve-test
  (is (true? (solve 0 [0 0 0] 3 3 [[0 1 0] [1 0 1] [0 1 0]])))
  (is (true? (solve 0 [0 0 0] 2 3 [[0 1 0] [1 0 1] [0 1 0]]))))

(deftest graph-coloring-test
  (is (true? (graph-coloring [[0 1 1] [1 0 1] [1 1 0]] 3 3)))
  (is (false? (graph-coloring [[0 1 1] [1 0 1] [1 1 0]] 2 3)))
  (is (true? (graph-coloring [[0 1] [1 0]] 2 2)))
  (is (false? (graph-coloring [[0 1] [1 0]] 1 2))))
