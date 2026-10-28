(ns clojure-algorithms.graphs.edmonds-karp-multiple-source-and-sink-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.edmonds-karp-multiple-source-and-sink
             :refer [find-maximum-flow]]))

(deftest test-maximum-flow
  (is (= 6 (find-maximum-flow [[0 7 0 0] [0 0 6 0] [0 0 0 8] [9 0 0 0]]
                              [0] [3])))
  (is (= 4 (find-maximum-flow [[0 3 2 0] [0 0 0 2] [0 0 0 3] [0 0 0 0]]
                              [0] [3])))
  (is (= 0 (find-maximum-flow [[0 0] [0 0]] [] [1])))
  (is (= 9 (find-maximum-flow [[0 0 4] [0 0 5] [0 0 0]] [0 1] [2]))))
