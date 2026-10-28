(ns clojure-algorithms.data-structures.stacks.dijkstras-two-stack-algorithm-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.dijkstras-two-stack-algorithm
             :refer [dijkstras-two-stack-algorithm]]))

(deftest test-dijkstra
  (is (= 8 (dijkstras-two-stack-algorithm "(5 + 3)")))
  (is (= 5 (dijkstras-two-stack-algorithm "((9 - (2 + 9)) + (8 - 1))")))
  (is (= -3 (dijkstras-two-stack-algorithm "((((3 - 2) - (2 + 3)) + (2 - 4)) + 3)")))
  (is (= 45 (dijkstras-two-stack-algorithm "(5 + ((4 * 2) * (2 + 3)))"))))
