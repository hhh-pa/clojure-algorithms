(ns clojure-algorithms.data-structures.binary-tree.is-sum-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.is-sum-tree
             :refer [make-node inorder-values tree-count sum-node? sum-tree?
                     build-a-tree build-a-sum-tree]]))

(deftest test-is-sum-tree
  (is (= [2] (vec (inorder-values (make-node 2)))))
  (is (= [1 2] (vec (inorder-values (make-node 2 (make-node 1) nil)))))
  (is (= 1 (tree-count (make-node 2))))
  (is (= 2 (tree-count (make-node 2 (make-node 1) nil))))
  (is (true? (sum-node? (make-node 3))))
  (is (false? (sum-node? (make-node 3 (make-node 1) nil))))
  (is (true? (sum-node? (make-node 3 (make-node 1) (make-node 2)))))
  (is (false? (sum-tree? (build-a-tree))))
  (is (true? (sum-tree? (build-a-sum-tree))))
  (is (= [1 2 7 11 15 29 35 40]
         (vec (inorder-values (build-a-tree))))))
