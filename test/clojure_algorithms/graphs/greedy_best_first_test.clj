(ns clojure-algorithms.graphs.greedy-best-first-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.greedy-best-first :refer [search]]))

(def grid [[0 0 1 0 0] [0 1 0 0 0] [0 0 1 0 1]
           [1 0 0 1 1] [0 0 0 0 0]])

(deftest test-greedy-best-first
  (is (= [[0 0] [1 0] [2 0] [2 1] [3 1] [4 1] [4 2] [4 3] [4 4]]
         (search grid [0 0] [4 4])))
  (is (= [[0 0]] (search [[0 1] [1 0]] [0 0] [1 1]))))
