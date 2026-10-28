(ns clojure-algorithms.graphs.prim-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.prim :refer [make-graph connect prim prim-heap]]))

(def demo
  (reduce (fn [g [a b w]] (connect g a b w))
          (make-graph 5)
          [[1 2 15] [1 3 12] [2 4 13] [2 5 5] [3 2 6] [3 4 6]]))

(deftest test-prim
  (is (= [[2 3] [3 1] [4 3] [5 2]] (prim demo 0)))
  (is (= [[2 3] [3 1] [4 3] [5 2]] (prim-heap demo 0))))
