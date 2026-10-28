(ns clojure-algorithms.data-structures.binary-tree.basic-binary-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.basic-binary-tree
             :refer [make-node inorder-values tree-count small-tree medium-tree
                     tree-depth full-tree?]]))

(deftest test-basic-binary-tree
  (is (= 3 (tree-count (small-tree))))
  (is (= [1 2 3] (vec (inorder-values (small-tree)))))
  (is (= 7 (tree-count (medium-tree))))
  (is (= [1 2 3 4 5 6 7] (vec (inorder-values (medium-tree)))))
  (is (= 1 (tree-depth (make-node 1))))
  (is (= 2 (tree-depth (small-tree))))
  (is (= 4 (tree-depth (medium-tree))))
  (is (true? (full-tree? (make-node 1))))
  (is (true? (full-tree? (small-tree))))
  (is (false? (full-tree? (medium-tree)))))
