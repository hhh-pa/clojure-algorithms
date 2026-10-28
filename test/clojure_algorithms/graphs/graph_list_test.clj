(ns clojure-algorithms.graphs.graph-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.graph-list :refer [make-graph add-edge]]))

(deftest test-graph-list
  (is (= {} (:adj (make-graph))))
  (let [g (-> (make-graph)
              (add-edge 0 1) (add-edge 1 2) (add-edge 1 4)
              (add-edge 1 5) (add-edge 2 0) (add-edge 2 6)
              (add-edge 2 7))]
    (is (= {0 [1] 1 [2 4 5] 2 [0 6 7] 4 [] 5 [] 6 [] 7 []} (:adj g))))
  (let [g (-> (make-graph false)
              (add-edge 0 1) (add-edge 1 2) (add-edge 1 4)
              (add-edge 1 5) (add-edge 2 0) (add-edge 2 6)
              (add-edge 2 7) (add-edge 4 5))]
    (is (= {0 [1 2] 1 [0 2 4 5] 2 [1 0 6 7] 4 [1 5] 5 [1 4]
            6 [2] 7 [2]}
           (:adj g)))))
