(ns clojure-algorithms.graphs.dijkstra-alternate-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.dijkstra-alternate
             :refer [make-graph set-edge minimum-distance dijkstra inf-dist]]))

(deftest test-dijkstra-alternate
  (is (= 0 (minimum-distance (make-graph 3) [1 2 3] [false false true])))
  (is (= [inf-dist 0 inf-dist inf-dist] (dijkstra (make-graph 4) 1)))
  (let [g (-> (make-graph 3)
              (set-edge 0 1 4) (set-edge 0 2 2)
              (set-edge 2 1 1))]
    (is (= [0 3 2] (dijkstra g 0)))))
