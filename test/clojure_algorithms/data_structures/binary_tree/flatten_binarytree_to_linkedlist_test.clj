(ns clojure-algorithms.data-structures.binary-tree.flatten-binarytree-to-linkedlist-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.flatten-binarytree-to-linkedlist
             :refer [make-node build-tree flatten-tree chain-values]]))

(deftest test-flatten
  (let [root (build-tree)]
    (is (= 1 (:data root)))
    (is (= 2 (:data (:left root))))
    (is (= 5 (:data (:right root))))
    (is (= 3 (:data (:left (:left root)))))
    (is (= 4 (:data (:right (:left root)))))
    (is (= 6 (:data (:right (:right root))))))
  (let [flat (flatten-tree (build-tree))]
    (is (= [1 2 3 4 5 6] (chain-values flat)))
    (is (every? nil? (map :left (take 6 (iterate :right flat))))))
  (is (= [1] (chain-values (flatten-tree (make-node 1)))))
  (is (= [] (chain-values (flatten-tree nil)))))
