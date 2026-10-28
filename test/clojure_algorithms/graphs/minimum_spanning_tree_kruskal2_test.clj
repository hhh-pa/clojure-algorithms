(ns clojure-algorithms.graphs.minimum-spanning-tree-kruskal2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.minimum-spanning-tree-kruskal2
             :refer [make-graph add-edge kruskal]]))

(deftest test-kruskal2
  (let [g (-> (make-graph)
              (add-edge 1 2 1) (add-edge 2 3 2) (add-edge 3 4 1)
              (add-edge 3 5 100) (add-edge 4 5 5))
        mst (kruskal g)]
    (is (contains? (get-in g [:connections 3]) 5))
    (is (not (contains? (get-in mst [:connections 3]) 5)))
    (is (= #{1 2 3 4 5} (set (keys (:connections mst))))))
  (let [g (-> (make-graph)
              (add-edge "A" "B" 1) (add-edge "B" "C" 2)
              (add-edge "C" "D" 1) (add-edge "C" "E" 100)
              (add-edge "D" "E" 5))
        mst (kruskal g)]
    (is (not (contains? (get-in mst [:connections "C"]) "E")))))
