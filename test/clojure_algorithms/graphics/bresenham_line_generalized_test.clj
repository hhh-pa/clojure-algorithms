(ns clojure-algorithms.graphics.bresenham-line-generalized-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphics.bresenham-line-generalized
             :refer [generalized-bresenham-line]]))

(deftest test-generalized-bresenham-line
  (is (= [[0 0] [1 1] [2 1] [3 2] [4 2] [5 3]]
         (generalized-bresenham-line [0 0] [5 3])))
  (is (= [[5 5] [4 4] [3 4] [2 3]]
         (generalized-bresenham-line [5 5] [2 3])))
  (is (= [[0 0] [0 1] [0 2]] (generalized-bresenham-line [0 0] [0 2]))))
