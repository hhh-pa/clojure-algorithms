(ns clojure-algorithms.graphs.breadth-first-search-shortest-path-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.breadth-first-search-shortest-path
             :refer [bfs-parents shortest-path]]))

(def demo {"A" ["B" "C" "E"] "B" ["A" "D" "E"] "C" ["A" "F" "G"]
           "D" ["B"] "E" ["A" "B" "D"] "F" ["C"] "G" ["C"]})

(deftest test-bfs-shortest-path
  (is (= {"G" nil "C" "G" "A" "C" "F" "C" "B" "A" "E" "A" "D" "B"}
         (bfs-parents demo "G")))
  (is (= "G->C->A->B->D" (shortest-path demo "G" "D")))
  (is (= "G" (shortest-path demo "G" "G")))
  (is (thrown? IllegalArgumentException (shortest-path demo "G" "Foo"))))
