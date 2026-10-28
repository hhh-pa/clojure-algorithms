(ns clojure-algorithms.graphs.dijkstra-binary-grid-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.dijkstra-binary-grid :refer [dijkstra]]))

(deftest test-dijkstra-binary-grid
  (is (= [4.0 [[0 0] [0 1] [1 1] [2 1] [2 2]]]
         (dijkstra [[1 1 1] [0 1 0] [0 1 1]] [0 0] [2 2] false)))
  (is (= [2.0 [[0 0] [1 1] [2 2]]]
         (dijkstra [[1 1 1] [0 1 0] [0 1 1]] [0 0] [2 2] true)))
  (is (= [4.0 [[0 0] [0 1] [0 2] [1 2] [2 2]]]
         (dijkstra [[1 1 1] [0 0 1] [0 1 1]] [0 0] [2 2] false)))
  (is (= [Double/POSITIVE_INFINITY []]
         (dijkstra [[1 0] [0 1]] [0 0] [1 1] false))))
