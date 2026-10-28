(ns clojure-algorithms.greedy-methods.activity-selection-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.greedy-methods.activity-selection :refer [activity-selection]]))

(deftest test-activity-selection
  (is (= [[1 3] [6 8]] (activity-selection [[1 3] [2 5] [3 9] [6 8]])))
  (is (= [[1 4] [5 7] [8 9]]
         (activity-selection [[0 6] [1 4] [3 5] [5 7] [5 9] [8 9]])))
  (is (= [[1 2] [2 4]] (activity-selection [[1 2] [2 4] [3 5] [0 6]])))
  (is (= [[1 2] [3 4] [5 9]]
         (activity-selection [[5 9] [1 2] [3 4] [0 6]])))
  (is (= [] (activity-selection [])))
  (is (= [] (activity-selection nil)))
  (is (= [[1 2]] (activity-selection [[1 2]]))))
