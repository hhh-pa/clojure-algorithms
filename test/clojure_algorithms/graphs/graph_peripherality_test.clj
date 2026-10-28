(ns clojure-algorithms.graphs.graph-peripherality-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.graph-peripherality
             :refer [floyd-warshall-algorithm find-peripheral-and-far-node
                     inf]]))

(deftest test-graph-peripherality
  (is (= [[0 0.0] [0 0.0]]
         (find-peripheral-and-far-node
          (floyd-warshall-algorithm {0 []}))))
  (is (= [[0 5.0] [0 5.0]]
         (find-peripheral-and-far-node
          (floyd-warshall-algorithm {0 [[1 5.0]] 1 [[0 5.0]]}))))
  (is (= [[0 1.0] [0 2.0]]
         (find-peripheral-and-far-node
          (floyd-warshall-algorithm
           {0 [[1 1.0] [2 1.0]] 1 [[0 1.0] [2 1.0]]
            2 [[0 1.0] [1 1.0]]}))))
  (is (= [[-1 inf] [-1 inf]]
         (find-peripheral-and-far-node
          (floyd-warshall-algorithm {0 [] 1 []}))))
  (is (thrown? IllegalArgumentException
               (floyd-warshall-algorithm {0 [[1 -2]] 1 []}))))
