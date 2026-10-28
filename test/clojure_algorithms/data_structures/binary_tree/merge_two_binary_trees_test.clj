(ns clojure-algorithms.data-structures.binary-tree.merge-two-binary-trees-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.merge-two-binary-trees
             :refer [make-node merge-trees preorder-values]]))

(deftest test-merge-trees
  (let [t1 (make-node 5 (make-node 6 (make-node 2) nil) (make-node 7))
        t2 (make-node 4 (make-node 5 nil (make-node 1))
                      (make-node 8 nil (make-node 4)))]
    (is (= [9 11 2 1 15 4] (vec (preorder-values (merge-trees t1 t2))))))
  (let [root (make-node 1 (make-node 2) (make-node 3))]
    (is (= [1 2 3] (vec (preorder-values root))))
    (is (= [3] (vec (preorder-values (:right root))))))
  (is (nil? (merge-trees nil nil)))
  (is (= [5] (vec (preorder-values (merge-trees (make-node 5) nil))))))
