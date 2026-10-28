(ns clojure-algorithms.data-structures.binary-tree.inorder-tree-traversal-2022-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.inorder-tree-traversal-2022
             :refer [bst-insert inorder-values make-tree]]))

(deftest test-inorder-2022
  (let [node-a (bst-insert nil 12345)
        node-b (bst-insert node-a 67890)]
    (is (= (:left node-a) (:left node-b)))
    (is (= 67890 (:data (:right node-b)))))
  (is (= [6 10 14 15 20 25 60] (vec (inorder-values (make-tree))))))
