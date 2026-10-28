(ns clojure-algorithms.graphs.graph-adjacency-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.graph-adjacency-list
             :refer [make-graph add-vertex add-edge remove-vertex remove-edge
                     contains-vertex? contains-edge? clear-graph]]))

(deftest test-adjacency-list
  (let [g (make-graph [] [] false)]
    (is (= {"A" []} (:adj (add-vertex g "A"))))
    (is (thrown? IllegalArgumentException
                 (add-vertex (add-vertex g "A") "A"))))
  (let [g (-> (make-graph ["A" "B"] [] false)
              (add-edge "A" "B"))]
    (is (true? (contains-edge? g "A" "B")))
    (is (true? (contains-edge? g "B" "A")))
    (is (thrown? IllegalArgumentException (add-edge g "A" "B")))
    (is (false? (contains-edge? (remove-edge g "A" "B") "A" "B"))))
  (let [g (-> (make-graph ["A" "B"] [])
              (add-edge "A" "B"))]
    (is (true? (contains-edge? g "A" "B")))
    (is (false? (contains-edge? g "B" "A"))))
  (let [g (remove-vertex (-> (make-graph ["A" "B"] [])
                             (add-edge "A" "B"))
                         "B")]
    (is (false? (contains-vertex? g "B")))
    (is (= {"A" []} (:adj g))))
  (is (= {} (:adj (clear-graph (make-graph ["A"] [])))))
  (is (thrown? IllegalArgumentException
               (contains-edge? (make-graph ["A"] []) "A" "Z"))))
