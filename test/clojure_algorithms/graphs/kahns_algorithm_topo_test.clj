(ns clojure-algorithms.graphs.kahns-algorithm-topo-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.kahns-algorithm-topo
             :refer [topological-sort]]))

(deftest test-kahns-topo
  (is (= [0 1 2 3 4 5]
         (topological-sort {0 [1 2] 1 [3] 2 [3] 3 [4 5] 4 [] 5 []})))
  (is (nil? (topological-sort {0 [1] 1 [2] 2 [0]})))
  (is (= [10 20] (topological-sort {10 [20] 20 []})))
  (is (nil? (topological-sort {10 [20] 20 [10]}))))
