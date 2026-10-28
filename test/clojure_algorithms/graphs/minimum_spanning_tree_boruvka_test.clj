(ns clojure-algorithms.graphs.minimum-spanning-tree-boruvka-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.minimum-spanning-tree-boruvka
             :refer [make-graph add-edge build-graph distinct-weight
                     boruvka-mst]]))

(deftest test-boruvka-mst
  (let [g (distinct-weight
           (build-graph [0 1 2 3] [[0 1 1] [0 2 1] [2 3 1]]))
        mst (boruvka-mst g)]
    (is (= 1 (get-in mst [:adjacency 0 1])))
    (is (= 2 (get-in mst [:adjacency 0 2])))
    (is (= 3 (get-in mst [:adjacency 2 3]))))
  (let [mst (boruvka-mst (build-graph [] [[0 1 4] [0 2 3] [1 2 2]]))]
    (is (= #{0 1 2} (set (keys (:adjacency mst)))))
    (is (= 5 (reduce + (for [[t ns] (:adjacency mst) [h w] ns
                             :when (< t h)]
                         w))))))
