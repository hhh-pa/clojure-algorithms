(ns clojure-algorithms.graphs.bi-directional-dijkstra-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.bi-directional-dijkstra
             :refer [bidirectional-dij]]))

(def fwd {"B" [["C" 1]] "C" [["D" 1]] "D" [["F" 1]]
          "E" [["B" 1] ["G" 2]] "F" [] "G" [["F" 1]]})
(def bwd {"B" [["E" 1]] "C" [["B" 1]] "D" [["C" 1]]
          "F" [["D" 1] ["G" 1]] "E" [[nil Double/POSITIVE_INFINITY]]
          "G" [["E" 2]]})

(deftest test-bidirectional-dij
  (is (= 3 (bidirectional-dij "E" "F" fwd bwd)))
  (is (= 0 (bidirectional-dij "E" "E" fwd bwd)))
  (is (= -1 (bidirectional-dij "F" "E" fwd bwd))))
