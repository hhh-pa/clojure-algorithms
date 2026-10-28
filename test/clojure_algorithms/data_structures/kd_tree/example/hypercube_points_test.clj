(ns clojure-algorithms.data-structures.kd-tree.example.hypercube-points-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.kd-tree.example.hypercube-points
             :refer [hypercube-points]]))

(deftest test-hypercube-points
  (let [pts (hypercube-points 10 10.0 2)]
    (is (= 10 (count pts)))
    (is (every? #(= 2 (count %)) pts))
    (is (every? (fn [p] (every? #(and (<= 0 %) (< % 10.0)) p)) pts)))
  (let [pts (hypercube-points 5 1.0 3)]
    (is (= 5 (count pts)))
    (is (every? #(= 3 (count %)) pts))))
