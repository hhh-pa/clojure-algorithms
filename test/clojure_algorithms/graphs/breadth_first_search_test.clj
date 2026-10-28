(ns clojure-algorithms.graphs.breadth-first-search-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.breadth-first-search
             :refer [make-graph add-edge bfs]]))

(deftest test-bfs
  (let [g (reduce (fn [g [f t]] (add-edge g f t))
                  (make-graph)
                  [[0 1] [0 1] [0 2] [1 2] [2 0] [2 3] [3 3]])]
    (is (= [0 1 2 3] (sort (bfs g 2))))
    (is (= #{3} (bfs g 3)))))
