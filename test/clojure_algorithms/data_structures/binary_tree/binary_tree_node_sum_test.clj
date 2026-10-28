(ns clojure-algorithms.data-structures.binary-tree.binary-tree-node-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-tree-node-sum
             :refer [make-node node-sum]]))

(deftest test-node-sum
  (is (= 10 (node-sum (make-node 10))))
  (is (= 15 (node-sum (make-node 10 (make-node 5) nil))))
  (is (= 12 (node-sum (make-node 10 (make-node 5) (make-node -3)))))
  (is (= 24 (node-sum (make-node 10 (make-node 5 (make-node 12) nil)
                                 (make-node -3)))))
  (is (= 32 (node-sum (make-node 10 (make-node 5 (make-node 12) nil)
                                 (make-node -3 (make-node 8) (make-node 0))))))
  (is (= 0 (node-sum nil))))
