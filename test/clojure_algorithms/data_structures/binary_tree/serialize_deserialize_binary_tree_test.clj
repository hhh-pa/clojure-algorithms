(ns clojure-algorithms.data-structures.binary-tree.serialize-deserialize-binary-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.serialize-deserialize-binary-tree
             :refer [make-node preorder-nodes node-count serialize five-tree
                     deserialize]]))

(deftest test-serialize
  (is (= [1] (mapv :value (preorder-nodes (make-node 1)))))
  (is (= 1 (node-count (make-node 1))))
  (is (= 3 (node-count (make-node 1 (make-node 2) (make-node 3)))))
  (is (= "1,null,null" (serialize (make-node 1))))
  (is (= "1,2,null,null,3,null,null"
         (serialize (make-node 1 (make-node 2) (make-node 3)))))
  (is (= "1,2,null,null,3,4,null,null,5,null,null"
         (serialize (make-node 1 (make-node 2)
                               (make-node 3 (make-node 4) (make-node 5))))))
  (is (= "1,2,null,null,3,4,null,null,5,null,null"
         (serialize (five-tree))))
  (is (thrown? clojure.lang.ExceptionInfo (make-node "a"))))

(deftest test-deserialize
  (let [root (five-tree)]
    (is (= root (deserialize (serialize root)))))
  (is (= (make-node 1) (deserialize "1,null,null")))
  (is (thrown? clojure.lang.ExceptionInfo (deserialize ""))))
