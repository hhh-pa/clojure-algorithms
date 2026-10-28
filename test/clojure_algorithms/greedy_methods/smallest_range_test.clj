(ns clojure-algorithms.greedy-methods.smallest-range-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.smallest-range :refer [smallest-range]]))

(deftest test-smallest-range
  (is (= [20 24] (smallest-range [[4 10 15 24 26] [0 9 12 20] [5 18 22 30]])))
  (is (= [1 1] (smallest-range [[1 2 3] [1 2 3] [1 2 3]])))
  (is (= [-1 1] (smallest-range [[-3 -2 -1] [0 0 0] [1 2 3]])))
  (is (= [3 7] (smallest-range [[1 2 3] [4 5 6] [7 8 9]])))
  (is (= [0 0] (smallest-range [[0 0 0] [0 0 0] [0 0 0]]))))
