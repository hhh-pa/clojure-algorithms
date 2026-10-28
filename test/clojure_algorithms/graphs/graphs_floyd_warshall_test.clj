(ns clojure-algorithms.graphs.graphs-floyd-warshall-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.graphs-floyd-warshall
             :refer [floyd-warshall inf]]))

(deftest test-floyd-warshall
  (let [[d _] (floyd-warshall [[0 inf inf] [inf 0 inf] [inf inf 0]] 3)]
    (is (= [[0 inf inf] [inf 0 inf] [inf inf 0]] d)))
  (let [[d _] (floyd-warshall [[0 3 inf] [2 0 inf] [inf 7 0]] 3)]
    (is (= [[0 3 inf] [2 0 inf] [9 7 0]] d)))
  (let [[d _] (floyd-warshall [[0 1 inf] [inf 0 2] [inf inf 0]] 3)]
    (is (= [[0 1 3] [inf 0 2] [inf inf 0]] d))))
