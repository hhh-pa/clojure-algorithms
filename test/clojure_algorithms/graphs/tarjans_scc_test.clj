(ns clojure-algorithms.graphs.tarjans-scc-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.tarjans-scc
             :refer [tarjan create-graph]]))

(deftest test-tarjan
  (is (= [[4 3 1 2 0]]
         (tarjan [[2 3 4] [2 3 4] [0 1 3] [0 1 2] [1]])))
  (is (= [[0] [1] [2] [3]] (tarjan [[] [] [] []])))
  (is (= [[1 3] [2] [0] [1 4] [5 6] [] [5]]
         (create-graph 7 [[0 1] [0 3] [1 2] [2 0] [3 1] [3 4]
                          [4 5] [4 6] [6 5]]))))
