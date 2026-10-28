(ns clojure-algorithms.graphs.breadth-first-search-shortest-path-2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.breadth-first-search-shortest-path-2
             :refer [bfs-shortest-path bfs-shortest-path-distance]]))

(def demo {"A" ["B" "C" "E"] "B" ["A" "D" "E"] "C" ["A" "F" "G"]
           "D" ["B"] "E" ["A" "B" "D"] "F" ["C"] "G" ["C"]})

(deftest test-bfs-shortest-path-2
  (is (= ["G" "C" "A" "B" "D"] (bfs-shortest-path demo "G" "D")))
  (is (= ["G"] (bfs-shortest-path demo "G" "G")))
  (is (= [] (bfs-shortest-path demo "G" "Unknown")))
  (is (= 4 (bfs-shortest-path-distance demo "G" "D")))
  (is (= 0 (bfs-shortest-path-distance demo "A" "A")))
  (is (= -1 (bfs-shortest-path-distance demo "A" "Unknown"))))
