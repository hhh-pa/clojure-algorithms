(ns clojure-algorithms.data-structures.binary-tree.diameter-of-binary-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.diameter-of-binary-tree
             :refer [make-node node-depth node-diameter]]))

(deftest test-depth-diameter
  (is (= 1 (node-depth (make-node 1))))
  (is (= 2 (node-depth (make-node 1 (make-node 2) nil))))
  (is (= 1 (node-depth (make-node 2))))
  (is (= 2 (node-depth (make-node 1 (make-node 2) (make-node 3)))))
  (is (= 1 (node-diameter (make-node 1))))
  (is (= 2 (node-diameter (make-node 1 (make-node 2) nil))))
  (is (= 1 (node-diameter (make-node 2))))
  (is (= 3 (node-diameter (make-node 1 (make-node 2) (make-node 3)))))
  (let [root (make-node 1 (make-node 2 (make-node 4) (make-node 5))
                        (make-node 3))]
    (is (= 4 (node-diameter root)))
    (is (= 3 (node-diameter (:left root))))
    (is (= 1 (node-diameter (:right root))))))
