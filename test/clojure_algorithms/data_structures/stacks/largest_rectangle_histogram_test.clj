(ns clojure-algorithms.data-structures.stacks.largest-rectangle-histogram-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.largest-rectangle-histogram
             :refer [largest-rectangle-area]]))

(deftest test-histogram
  (is (= 10 (largest-rectangle-area [2 1 5 6 2 3])))
  (is (= 4 (largest-rectangle-area [2 4])))
  (is (= 12 (largest-rectangle-area [6 2 5 4 5 1 6])))
  (is (= 1 (largest-rectangle-area [1])))
  (is (= 0 (largest-rectangle-area []))))
