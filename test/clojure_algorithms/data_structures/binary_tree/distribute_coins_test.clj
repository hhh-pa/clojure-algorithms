(ns clojure-algorithms.data-structures.binary-tree.distribute-coins-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.distribute-coins
             :refer [make-node distribute-coins]]))

(deftest test-distribute-coins
  (is (= 2 (distribute-coins (make-node 3 (make-node 0) (make-node 0)))))
  (is (= 3 (distribute-coins (make-node 0 (make-node 3) (make-node 0)))))
  (is (= 3 (distribute-coins (make-node 0 (make-node 0) (make-node 3)))))
  (is (= 0 (distribute-coins nil)))
  (is (= 0 (distribute-coins (make-node 1))))
  (is (thrown? clojure.lang.ExceptionInfo
               (distribute-coins (make-node 0 (make-node 0) (make-node 0)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (distribute-coins (make-node 0 (make-node 1) (make-node 1))))))
