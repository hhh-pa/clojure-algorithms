(ns clojure-algorithms.data-structures.binary-tree.binary-tree-path-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.binary-tree-path-sum
             :refer [make-node path-sum-count]]))

(defn- make-tree []
  (make-node 10
             (make-node 5 (make-node 3 (make-node 3) (make-node -2))
                        (make-node 2 nil (make-node 1)))
             (make-node -3 nil (make-node 11))))

(deftest test-path-sum-count
  (is (= 3 (path-sum-count (make-tree) 8)))
  (is (= 2 (path-sum-count (make-tree) 7)))
  (let [t (assoc-in (make-tree) [:right :right] (make-node 10))]
    (is (= 2 (path-sum-count t 8))))
  (is (= 0 (path-sum-count nil 0)))
  (is (= 0 (path-sum-count (make-tree) 0)))
  (is (= 4 (path-sum-count (make-node 0 (make-node 5) (make-node 5)) 5))))
