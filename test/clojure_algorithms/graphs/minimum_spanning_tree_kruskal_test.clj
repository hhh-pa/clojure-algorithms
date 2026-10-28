(ns clojure-algorithms.graphs.minimum-spanning-tree-kruskal-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.minimum-spanning-tree-kruskal
             :refer [kruskal]]))

(deftest test-kruskal
  (is (= [[2 3 1] [0 1 3] [1 2 5]]
         (kruskal 4 [[0 1 3] [1 2 5] [2 3 1]])))
  (is (= [[2 3 1] [0 2 1] [0 1 3]]
         (kruskal 4 [[0 1 3] [1 2 5] [2 3 1] [0 2 1] [0 3 2]])))
  (is (= [[2 3 1] [0 2 1] [2 1 1]]
         (kruskal 4 [[0 1 3] [1 2 5] [2 3 1] [0 2 1] [0 3 2] [2 1 1]]))))
