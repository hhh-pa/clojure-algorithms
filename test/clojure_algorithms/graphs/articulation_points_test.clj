(ns clojure-algorithms.graphs.articulation-points-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.articulation-points :refer [compute-ap]]))

(deftest test-articulation-points
  (is (= [2 3 5]
         (compute-ap {0 [1 2] 1 [0 2] 2 [0 1 3 5] 3 [2 4] 4 [3]
                      5 [2 6 8] 6 [5 7] 7 [6 8] 8 [5 7]})))
  (is (= [] (compute-ap {0 [1 2] 1 [0 2] 2 [0 1]})))
  (is (= [1] (compute-ap {0 [1] 1 [0 2] 2 [1]}))))
