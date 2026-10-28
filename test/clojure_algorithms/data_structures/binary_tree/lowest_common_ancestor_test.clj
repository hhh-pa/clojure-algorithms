(ns clojure-algorithms.data-structures.binary-tree.lowest-common-ancestor-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.lowest-common-ancestor
             :refer [swap-pair create-sparse lowest-common-ancestor
                     breadth-first-search sample-graph-data]]))

(deftest test-swap
  (is (= [3 2] (swap-pair 2 3)))
  (is (= [4 3] (swap-pair 3 4)))
  (is (= [12 67] (swap-pair 67 12)))
  (is (= [-4 3] (swap-pair 3 -4))))

(deftest test-create-sparse
  (let [parent (vec (cons (vec [0 0 1 1 2 2 3])
                          (repeat 19 (vec (repeat 7 0)))))
        p (create-sparse 6 parent)]
    (is (= [0 0 1 1 2 2 3] (nth p 0)))
    (is (= [0 0 0 0 1 1 1] (nth p 1)))
    (is (= [0 0 0 0 0 0 0] (nth p 2))))
  (let [parent (vec (cons (vec [0 0]) (repeat 19 (vec (repeat 2 0)))))
        p (create-sparse 1 parent)]
    (is (= [0 0] (nth p 0)))
    (is (= [0 0] (nth p 1)))))

(deftest test-lca-sample
  (let [[level parent] (sample-graph-data)]
    (is (= 1 (lowest-common-ancestor 1 3 level parent)))
    (is (= 1 (lowest-common-ancestor 5 6 level parent)))
    (is (= 3 (lowest-common-ancestor 7 11 level parent)))
    (is (= 3 (lowest-common-ancestor 6 7 level parent)))
    (is (= 4 (lowest-common-ancestor 4 12 level parent)))
    (is (= 8 (lowest-common-ancestor 8 8 level parent)))))
