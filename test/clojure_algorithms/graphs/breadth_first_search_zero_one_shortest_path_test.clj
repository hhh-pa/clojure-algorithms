(ns clojure-algorithms.graphs.breadth-first-search-zero-one-shortest-path-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.breadth-first-search-zero-one-shortest-path
             :refer [make-graph add-edge get-shortest-path]]))

(def demo
  (reduce (fn [g [f t w]] (add-edge g f t w))
          (make-graph 11)
          [[0 1 0] [0 3 1] [1 2 0] [2 3 0] [4 2 1] [4 5 1] [4 6 1]
           [5 9 0] [6 7 1] [7 8 1] [8 10 1] [9 7 0] [9 10 1]]))

(deftest test-zero-one-shortest-path
  (is (= 0 (get-shortest-path demo 0 3)))
  (is (= 2 (get-shortest-path demo 4 10)))
  (is (= 2 (get-shortest-path demo 4 8)))
  (is (= 0 (get-shortest-path demo 0 1)))
  (is (thrown? IllegalArgumentException (get-shortest-path demo 0 4)))
  (is (thrown? IllegalArgumentException (get-shortest-path demo 1 0)))
  (is (thrown? IllegalArgumentException (add-edge demo 1 2 2)))
  (is (thrown? IllegalArgumentException (add-edge (make-graph 2) 0 2 1))))
