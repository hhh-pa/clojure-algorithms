(ns clojure-algorithms.data-structures.binary-tree.symmetric-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.symmetric-tree
             :refer [make-node make-symmetric-tree make-asymmetric-tree
                     is-mirror? symmetric-tree?]]))

(deftest test-symmetric-tree
  (let [node (make-node 1 (make-node 2) (make-node 3))]
    (is (= 1 (:data node)))
    (is (= 2 (:data (:left node))))
    (is (= 3 (:data (:right node)))))
  (let [tree (make-symmetric-tree)]
    (is (= 1 (:data tree)))
    (is (= (:data (:left tree)) (:data (:right tree))))
    (is (= (:data (:left (:left tree))) (:data (:right (:right tree))))))
  (let [tree (make-asymmetric-tree)]
    (is (= 1 (:data tree)))
    (is (= (:data (:left tree)) (:data (:right tree))))
    (is (not= (:data (:left (:left tree))) (:data (:right (:right tree))))))
  (is (true? (symmetric-tree? (make-symmetric-tree))))
  (is (false? (symmetric-tree? (make-asymmetric-tree))))
  (is (true? (symmetric-tree? nil)))
  (let [t1 (make-symmetric-tree)]
    (is (true? (is-mirror? (:left t1) (:right t1)))))
  (let [t2 (make-asymmetric-tree)]
    (is (false? (is-mirror? (:left t2) (:right t2))))))
