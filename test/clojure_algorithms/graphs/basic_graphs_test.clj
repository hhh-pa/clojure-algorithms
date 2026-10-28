(ns clojure-algorithms.graphs.basic-graphs-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.basic-graphs
             :refer [dfs bfs dijk topo floyd-warshall prim krusk
                     find-isolated-nodes]]))

(def dijk-graph
  {1 [[2 7] [3 9] [6 14]] 2 [[1 7] [3 10] [4 15]]
   3 [[1 9] [2 10] [4 11] [6 2]] 4 [[2 15] [3 11] [5 6]]
   5 [[4 6] [6 9]] 6 [[1 14] [3 2] [5 9]]})

(deftest test-basic-graphs
  (is (= [1 2 4 5 3] (dfs {1 [2 3] 2 [4 5] 3 [] 4 [] 5 []} 1)))
  (is (= [1 2 3 4 5 6 7 8]
         (bfs {1 [2 3] 2 [4 5] 3 [6 7] 4 [] 5 [8] 6 [] 7 [] 8 []} 1)))
  (is (= {1 0 2 7 3 9 6 11 4 20 5 20} (dijk dijk-graph 1)))
  (is (= [1 2 3 4] (topo {1 [2 3] 2 [4] 3 [4] 4 []})))
  (is (= [[0 5 15] [50 0 10]
          [Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY 0]]
         (floyd-warshall [[[0 5 Double/POSITIVE_INFINITY]
                           [50 0 10]
                           [Double/POSITIVE_INFINITY
                            Double/POSITIVE_INFINITY 0]] 3])))
  (is (= {1 0 2 7 3 9 6 2 4 6 5 9} (prim dijk-graph 1)))
  (is (= [[1 2 1] [2 3 2]] (krusk [[[1 2 1] [2 3 2] [1 3 3]] 3])))
  (is (= [4] (find-isolated-nodes {1 [2 3] 2 [1 3] 3 [1 2] 4 []})))
  (is (= [] (find-isolated-nodes {}))))
