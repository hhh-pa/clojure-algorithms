(ns clojure-algorithms.graphs.connected-components-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.connected-components
             :refer [connected-components dfs]]))

(def g1 {0 [1 2] 1 [0 3] 2 [0] 3 [1] 4 [5 6] 5 [4 6] 6 [4 5]})
(def g2 {0 [1 2 3] 1 [0 3] 2 [0] 3 [0 1] 4 [] 5 []})

(deftest test-connected-components
  (is (= [0 1 3 2] (dfs g1 0)))
  (is (= [0 1 3 2] (dfs g2 0)))
  (is (= [[0 1 3 2] [4 5 6]] (connected-components g1)))
  (is (= [[0 1 3 2] [4] [5]] (connected-components g2))))
