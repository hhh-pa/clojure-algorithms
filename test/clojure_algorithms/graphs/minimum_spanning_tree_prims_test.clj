(ns clojure-algorithms.graphs.minimum-spanning-tree-prims-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.minimum-spanning-tree-prims
             :refer [prisms-algorithm]]))

(deftest test-prisms-algorithm
  (is (= [[0 1] [1 4] [4 3] [4 5] [5 2]]
         (prisms-algorithm
          {0 [[1 1] [3 3]] 1 [[0 1] [2 6] [3 5] [4 1]]
           2 [[1 6] [4 5] [5 2]] 3 [[0 3] [1 5] [4 1]]
           4 [[1 1] [2 5] [3 1] [5 4]] 5 [[2 2] [4 4]]}))))
