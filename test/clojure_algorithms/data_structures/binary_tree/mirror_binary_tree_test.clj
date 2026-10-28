(ns clojure-algorithms.data-structures.binary-tree.mirror-binary-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.mirror-binary-tree
             :refer [make-node inorder-values mirror-tree make-tree-seven
                     make-tree-nine]]))

(deftest test-mirror
  (is (= [0] (vec (inorder-values (make-node 0)))))
  (is (= [0] (vec (inorder-values (mirror-tree (make-node 0))))))
  (let [tree (make-node 1 (make-node 0)
                        (make-node 3 (make-node 2)
                                   (make-node 4 nil (make-node 5))))]
    (is (= [0 1 2 3 4 5] (vec (inorder-values tree))))
    (is (= [5 4 3 2 1 0] (vec (inorder-values (mirror-tree tree))))))
  (is (= 7 (count (inorder-values (make-tree-seven)))))
  (is (= [4 2 5 1 6 3 7] (vec (inorder-values (make-tree-seven)))))
  (is (= 9 (count (inorder-values (make-tree-nine)))))
  (is (= [7 4 8 2 5 9 1 3 6] (vec (inorder-values (make-tree-nine)))))
  (is (= [6 3 1 9 5 2 8 4 7]
         (vec (inorder-values (mirror-tree (make-tree-nine)))))))
