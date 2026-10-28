(ns clojure-algorithms.graphs.check-bipatrite-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.check-bipatrite
             :refer [is-bipartite-dfs is-bipartite-bfs]]))

(deftest test-bipartite
  (doseq [f [is-bipartite-dfs is-bipartite-bfs]]
    (is (true? (f {})))
    (is (true? (f {0 [1 2] 1 [0 3] 2 [0 4] 3 [1] 4 [2]})))
    (is (false? (f {0 [1 2] 1 [0 2] 2 [0 1]})))
    (is (true? (f {0 [1 3] 1 [0 2] 2 [1 3] 3 [0 2]})))
    (is (false? (f {0 [1 2 3] 1 [0 2] 2 [0 1 3] 3 [0 2]})))
    (is (true? (f {0 [4] 1 [] 2 [4] 3 [4] 4 [0 2 3]})))))
