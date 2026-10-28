(ns clojure-algorithms.graphs.graph-adjacency-matrix-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.graph-adjacency-matrix
             :refer [make-graph add-vertex add-edge remove-vertex remove-edge
                     contains-vertex? contains-edge? clear-graph]]))

(deftest test-adjacency-matrix
  (let [g (add-vertex (make-graph [] []) "A")]
    (is (= [[0]] (:matrix g)))
    (is (thrown? IllegalArgumentException (add-vertex g "A"))))
  (let [g (-> (make-graph ["A" "B"] [] false)
              (add-edge "A" "B"))]
    (is (true? (contains-edge? g "A" "B")))
    (is (true? (contains-edge? g "B" "A")))
    (is (thrown? IllegalArgumentException (add-edge g "A" "B"))))
  (let [g (-> (make-graph ["A" "B"] [])
              (add-edge "A" "B"))]
    (is (true? (contains-edge? g "A" "B")))
    (is (false? (contains-edge? g "B" "A")))
    (is (false? (contains-edge? (remove-edge g "A" "B") "A" "B"))))
  (let [g (remove-vertex (-> (make-graph ["A" "B" "C"] [])
                             (add-edge "A" "B") (add-edge "B" "C"))
                         "B")]
    (is (false? (contains-vertex? g "B")))
    (is (= [[0 0] [0 0]] (:matrix g))))
  (is (= [] (:matrix (clear-graph (make-graph ["A"] [])))))
  (is (thrown? IllegalArgumentException
               (contains-edge? (make-graph ["A"] []) "A" "Z"))))
