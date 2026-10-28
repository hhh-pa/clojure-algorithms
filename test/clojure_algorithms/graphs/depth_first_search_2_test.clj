(ns clojure-algorithms.graphs.depth-first-search-2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.depth-first-search-2
             :refer [make-graph add-edge dfs dfs-recursive]]))

(def demo (reduce (fn [g [f t]] (add-edge g f t))
                  (make-graph)
                  [[0 1] [0 2] [1 2] [2 0] [2 3] [3 3]]))

(deftest test-dfs-2
  (is (= #{0 1 2 3} (set (dfs demo))))
  (is (= 0 (first (dfs demo))))
  (is (= #{2 0 1 3} (set (dfs-recursive demo 2)))))
