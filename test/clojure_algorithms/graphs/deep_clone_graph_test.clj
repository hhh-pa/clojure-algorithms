(ns clojure-algorithms.graphs.deep-clone-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.deep-clone-graph :refer [clone-graph]]))

(deftest test-clone-graph
  (is (nil? (clone-graph nil nil)))
  (is (= {1 []} (clone-graph {1 []} 1)))
  (is (= {1 [2] 2 [1]} (clone-graph {1 [2] 2 [1]} 1)))
  (let [g {1 [2 3] 2 [1] 3 [1]}]
    (is (= g (clone-graph g 1)))))
