(ns clojure-algorithms.graphs.eulerian-path-and-circuit-for-undirected-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.eulerian-path-and-circuit-for-undirected-graph
             :refer [dfs check-circuit-or-path check-euler]]))

(deftest test-eulerian
  (is (= [1 2 3 1] (dfs 1 {1 [2 3] 2 [1 3] 3 [1 2]} #{})))
  (is (= [1] (dfs 1 {1 [] 2 []} #{})))
  (is (= [1 -1] (check-circuit-or-path {1 [2 3] 2 [1 3] 3 [1 2]} 10)))
  (is (= [2 5] (check-circuit-or-path
                {1 [2 3 4] 2 [] 3 [1 2] 4 [] 5 [4]} 10)))
  (is (= {:type :cycle :path [1 2 3 1]}
         (check-euler {1 [2 3] 2 [1 3] 3 [1 2]} 10)))
  (is (= {:type :path :path [5 4 1 2 3 1]}
         (check-euler {1 [2 3 4] 2 [1 3] 3 [1 2] 4 [1 5] 5 [4]} 10)))
  (is (= :none (:type (check-euler {1 [2 3 1] 2 [2 3 4] 3 [1 3]
                                    4 [1] 5 []} 10)))))
