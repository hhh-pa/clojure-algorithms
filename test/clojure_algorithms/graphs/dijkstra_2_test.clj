(ns clojure-algorithms.graphs.dijkstra-2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.dijkstra-2 :refer [dijkstra min-dist inf]]))

(def g [[0.0 5.0 inf 10.0] [inf 0.0 3.0 inf]
        [inf inf 0.0 1.0] [inf inf inf 0.0]])

(deftest test-dijkstra-2
  (is (= 2 (min-dist [0 4 2 inf] [true false false false] 4)))
  (is (= -1 (min-dist [0 4 2 1] [true true true true] 4)))
  (is (= [0.0 5.0 8.0 9.0] (dijkstra g 4 0)))
  (is (= [0.0 inf] (dijkstra [[0.0 inf] [inf 0.0]] 2 0)))
  (is (= [0.0] (dijkstra [[0.0]] 1 0)))
  (is (= [0.0 1.0 4.0]
         (dijkstra [[0.0 1.0 6.0] [inf 0.0 3.0] [inf inf 0.0]] 3 0))))
