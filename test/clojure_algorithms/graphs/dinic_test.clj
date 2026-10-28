(ns clojure-algorithms.graphs.dinic-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.dinic :refer [make-network add-edge max-flow]]))

(deftest test-dinic
  (let [g (make-network 10)]
    (doseq [v (range 1 5)] (add-edge g 0 v 1))
    (doseq [v (range 5 9)] (add-edge g v 9 1))
    (doseq [v (range 1 5)] (add-edge g v (+ v 4) 1))
    (is (= 4 (max-flow g 0 9))))
  (let [g (make-network 4)]
    (add-edge g 0 1 3) (add-edge g 0 2 2)
    (add-edge g 1 3 2) (add-edge g 2 3 3)
    (is (= 4 (max-flow g 0 3)))))
