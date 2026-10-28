(ns clojure-algorithms.graphics.bresenham-line-basic-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphics.bresenham-line-basic
             :refer [classic-bresenham-line]]))

(deftest test-classic-bresenham-line
  (is (= [[0 0] [1 1] [2 1] [3 2] [4 2] [5 3]]
         (classic-bresenham-line [0 0] [5 3])))
  (is (= [[0 0] [1 0] [2 0]] (classic-bresenham-line [0 0] [2 0]))))
