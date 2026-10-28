(ns clojure-algorithms.data-structures.kd-tree.kd-node-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.kd-tree.kd-node
             :refer [make-kd-node]]))

(deftest test-kd-node
  (let [n (make-kd-node [1.0 2.0])]
    (is (= [1.0 2.0] (:point n)))
    (is (nil? (:left n)))
    (is (nil? (:right n))))
  (let [n (make-kd-node [0.0] (make-kd-node [-1.0]) (make-kd-node [1.0]))]
    (is (= [-1.0] (:point (:left n))))
    (is (= [1.0] (:point (:right n))))))
