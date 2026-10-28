(ns clojure-algorithms.data-structures.binary-tree.is-sorted-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.is-sorted
             :refer [make-node inorder-values sorted-tree?]]))

(deftest test-is-sorted
  (is (= [2.1] (vec (inorder-values (make-node 2.1)))))
  (is (= [2.0 2.1] (vec (inorder-values (make-node 2.1 (make-node 2.0) nil)))))
  (is (= [2.0 2.1 2.2]
         (vec (inorder-values (make-node 2.1 (make-node 2.0) (make-node 2.2))))))
  (is (true? (sorted-tree? (make-node "abc"))))
  (is (true? (sorted-tree? (make-node 2 (make-node 1.999) (make-node 3)))))
  (is (true? (sorted-tree? (make-node 0 (make-node 0) (make-node 0)))))
  (is (true? (sorted-tree? (make-node 0 (make-node -11) (make-node 3)))))
  (is (false? (sorted-tree? (make-node 5 (make-node 1)
                                       (make-node 4 (make-node 3) nil))))))
