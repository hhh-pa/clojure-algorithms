(ns clojure-algorithms.data-structures.binary-tree.avl-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.avl-tree
             :refer [make-node insert-node avl-delete node-height get-left-most
                     get-right-most inorder-values]]))

(deftest test-avl-insert
  (let [t (-> nil (insert-node 4) (insert-node 2) (insert-node 3))]
    (is (= [2 3 4] (vec (inorder-values t))))
    (is (= 3 (:data t)))
    (is (= 2 (node-height t))))
  (let [t (reduce insert-node nil [10 20 30 40 50 25])]
    (is (= [10 20 25 30 40 50] (vec (inorder-values t))))
    (is (<= (node-height t) 4)))
  (let [t (reduce insert-node nil (range 1 16))]
    (is (= (range 1 16) (inorder-values t)))
    (is (<= (node-height t) 5))))

(deftest test-avl-delete
  (let [t (-> nil (insert-node 4) (insert-node 2) (insert-node 3)
              (avl-delete 3))]
    (is (= [2 4] (vec (inorder-values t))))
    (is (= 4 (:data t))))
  (let [t (reduce avl-delete (reduce insert-node nil (range 10)) [5 0 9])]
    (is (= [1 2 3 4 6 7 8] (vec (inorder-values t)))))
  (is (nil? (avl-delete nil 1))))

(deftest test-avl-min-max
  (let [t (reduce insert-node nil [5 3 8 1 4])]
    (is (= 1 (get-left-most t)))
    (is (= 8 (get-right-most t)))
    (is (= 0 (node-height nil)))
    (is (= 1 (node-height (make-node 9))))))
