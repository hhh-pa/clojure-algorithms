(ns clojure-algorithms.graphs.dijkstra-algorithm-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.dijkstra-algorithm
             :refer [make-graph add-edge dijkstra shortest-path inf]]))

(deftest test-dijkstra-algorithm
  (let [g (-> (make-graph 3) (add-edge 0 1 2) (add-edge 1 2 2))]
    (is (= [0 2 4] (:dist (dijkstra g 0)))))
  (let [g (-> (make-graph 4) (add-edge 0 1 4) (add-edge 1 2 2)
              (add-edge 2 3 1) (add-edge 0 2 3))]
    (is (= [0 4 3 4] (:dist (dijkstra g 0)))))
  (let [g (-> (make-graph 4) (add-edge 0 1 4) (add-edge 1 2 2)
              (add-edge 2 3 1) (add-edge 0 2 7))]
    (is (= [0 4 6 7] (:dist (dijkstra g 0)))))
  (let [g (-> (make-graph 3) (add-edge 0 1 2))]
    (is (= [0 2 inf] (:dist (dijkstra g 0)))))
  (let [g (-> (make-graph 4) (add-edge 0 1 1) (add-edge 1 2 2)
              (add-edge 2 3 3))
        res (dijkstra g 0)]
    (is (= {:path [0 1 2 3] :cost 6} (shortest-path g res 0 3)))))
