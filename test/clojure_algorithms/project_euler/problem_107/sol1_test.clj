(ns clojure-algorithms.project-euler.problem-107.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-107.sol1 :refer [add-edge
                                                                       make-graph
                                                                       prims-algorithm
                                                                       solution]]))

(def ^:private example-network
  "-,16,12,21,-,-,-\n16,-,-,17,20,-,-\n12,-,-,28,-,31,-\n21,17,28,-,18,19,23\n-,20,-,18,-,-,11\n-,-,31,19,-,-,27\n-,-,-,23,11,27,-")

(deftest add-edge-test
  (let [graph (add-edge (make-graph #{1 2} {[2 1] 4}) [3 1] 5)]
    (is (= [1 2 3] (sort (:vertices graph))))
    (is (= [[1 2] [1 3]] (sort (keys (:edges graph)))))
    (is (= 4 (get (:edges graph) [1 2])))
    (is (= 5 (get (:edges graph) [1 3])))))

(deftest prims-algorithm-test
  (let [mst (prims-algorithm
             (make-graph #{1 2 3 4}
                         {[1 2] 5 [1 3] 10 [1 4] 20 [2 4] 30 [3 4] 1}))]
    (is (= [1 2 3 4] (sort (:vertices mst))))
    (is (= [[1 2] [1 3] [3 4]] (sort (keys (:edges mst)))))))

(deftest solution-test
  (is (= 150 (solution example-network))))
