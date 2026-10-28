(ns clojure-algorithms.data-structures.binary-tree.binary-tree-traversals-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-tree-traversals
             :refer [make-tree preorder postorder inorder reverse-inorder
                     tree-height level-order nodes-left-to-right
                     nodes-right-to-left zigzag]]))

(deftest test-traversals
  (is (= [1 2 4 5 3] (vec (preorder (make-tree)))))
  (is (= [4 5 2 3 1] (vec (postorder (make-tree)))))
  (is (= [4 2 5 1 3] (vec (inorder (make-tree)))))
  (is (= [3 1 5 2 4] (vec (reverse-inorder (make-tree)))))
  (is (= 0 (tree-height nil)))
  (is (= 3 (tree-height (make-tree))))
  (is (= [1 2 3 4 5] (vec (level-order (make-tree)))))
  (is (= [1] (nodes-left-to-right (make-tree) 1)))
  (is (= [2 3] (nodes-left-to-right (make-tree) 2)))
  (is (= [1] (nodes-right-to-left (make-tree) 1)))
  (is (= [3 2] (nodes-right-to-left (make-tree) 2)))
  (is (= [1 3 2 4 5] (zigzag (make-tree))))
  (is (nil? (preorder nil))))
