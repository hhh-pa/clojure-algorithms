(ns clojure-algorithms.data-structures.binary-tree.maximum-sum-bst-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.maximum-sum-bst
             :refer [make-node max-sum-bst]]))

(deftest test-max-sum-bst
  (let [t1 (make-node 4 (make-node 3 (make-node 1) (make-node 2)) nil)]
    (is (= 2 (max-sum-bst t1))))
  (let [t2 (make-node -4 (make-node -2) (make-node -5))]
    (is (= 0 (max-sum-bst t2))))
  (let [t3 (make-node 1 (make-node 4 (make-node 2) (make-node 4))
                      (make-node 3 (make-node 2)
                                 (make-node 5 (make-node 4) (make-node 6))))]
    (is (= 20 (max-sum-bst t3))))
  (is (= 1 (max-sum-bst (make-node 1))))
  (is (= 0 (max-sum-bst nil))))
