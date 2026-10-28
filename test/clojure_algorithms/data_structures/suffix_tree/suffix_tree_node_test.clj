(ns clojure-algorithms.data-structures.suffix-tree.suffix-tree-node-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.suffix-tree.suffix-tree-node
             :refer [make-suffix-tree-node]]))

(deftest test-suffix-tree-node
  (let [n (make-suffix-tree-node)]
    (is (= {} (:children n)))
    (is (false? (:is-end-of-string n)))
    (is (nil? (:start n)))
    (is (nil? (:end n)))
    (is (nil? (:suffix-link n))))
  (let [n (make-suffix-tree-node {} true 0 5 nil)]
    (is (true? (:is-end-of-string n)))
    (is (= 0 (:start n)))
    (is (= 5 (:end n)))))
