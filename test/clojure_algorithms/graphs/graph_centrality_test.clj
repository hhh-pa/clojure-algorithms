(ns clojure-algorithms.graphs.graph-centrality-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.graph-centrality
             :refer [floyd-warshall-algorithm find-central-and-median-node
                     inf]]))

(deftest test-graph-centrality
  (is (= [[0 0.0] [0 0.0]]
         (find-central-and-median-node
          (floyd-warshall-algorithm {0 []}))))
  (is (= [[0 5.0] [0 0.2]]
         (find-central-and-median-node
          (floyd-warshall-algorithm {0 [[1 5.0]] 1 [[0 5.0]]}))))
  (is (= [[0 1.0] [0 2.0]]
         (find-central-and-median-node
          (floyd-warshall-algorithm
           {0 [[1 1.0] [2 1.0]] 1 [[0 1.0] [2 1.0]]
            2 [[0 1.0] [1 1.0]]}))))
  (is (= [[-1 inf] [-1 inf]]
         (find-central-and-median-node
          (floyd-warshall-algorithm {0 [] 1 []}))))
  (is (thrown? IllegalArgumentException
               (floyd-warshall-algorithm {0 [[1 0]] 1 []}))))
