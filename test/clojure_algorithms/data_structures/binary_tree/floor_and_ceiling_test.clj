(ns clojure-algorithms.data-structures.binary-tree.floor-and-ceiling-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.floor-and-ceiling
             :refer [make-node inorder-keys floor-ceiling]]))

(defn- make-test-tree []
  (make-node 10 (make-node 5 (make-node 3) (make-node 7))
             (make-node 20 (make-node 15) (make-node 25))))

(deftest test-floor-ceiling
  (let [root (make-test-tree)]
    (is (= [3 5 7 10 15 20 25] (vec (inorder-keys root))))
    (is (= [7 10] (floor-ceiling root 8)))
    (is (= [10 15] (floor-ceiling root 14)))
    (is (= [nil 3] (floor-ceiling root -1)))
    (is (= [25 nil] (floor-ceiling root 30)))
    (is (= [10 10] (floor-ceiling root 10))))
  (is (= [nil nil] (floor-ceiling nil 5))))
