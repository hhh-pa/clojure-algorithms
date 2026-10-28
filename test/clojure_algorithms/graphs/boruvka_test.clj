(ns clojure-algorithms.graphs.boruvka-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.boruvka :refer [make-graph add-edge boruvka]]))

(deftest test-boruvka
  (let [g (reduce (fn [g [u v w]] (add-edge g u v w))
                  (make-graph 8)
                  [[0 1 10] [0 2 6] [0 3 5] [1 3 15] [2 3 4] [3 4 8]
                   [4 5 10] [4 6 6] [4 7 5] [5 7 15] [6 7 4]])
        {:keys [edges weight]} (boruvka g)]
    (is (= 46 weight))
    (is (= 7 (count edges)))))
