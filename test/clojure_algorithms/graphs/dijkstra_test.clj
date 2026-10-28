(ns clojure-algorithms.graphs.dijkstra-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.dijkstra :refer [dijkstra]]))

(def g {"A" [["B" 2] ["C" 5]] "B" [["A" 2] ["D" 1]] "C" [["A" 5] ["D" 2]]
        "D" [["B" 1] ["C" 2] ["E" 3]] "E" [["D" 3]] "F" []})

(deftest test-dijkstra
  (is (= 6 (dijkstra g "A" "E")))
  (is (= 0 (dijkstra g "A" "A")))
  (is (= -1 (dijkstra g "A" "F"))))
