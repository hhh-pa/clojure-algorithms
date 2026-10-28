(ns clojure-algorithms.data-structures.binary-tree.binary-tree-maximum-path-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-tree-maximum-path-sum
             :refer [make-node construct-tree max-path-sum]]))

(deftest test-max-path-sum
  (let [root (make-node 10
                        (make-node 5 (make-node 3 (make-node 3) (make-node -2))
                                   (make-node 2 nil (make-node 1)))
                        (make-node -3 nil (make-node 11)))]
    (is (= 29 (max-path-sum root))))
  (is (= 42 (max-path-sum (construct-tree))))
  (is (= 5 (max-path-sum (make-node 5))))
  (is (= -3 (max-path-sum (make-node -3)))))
