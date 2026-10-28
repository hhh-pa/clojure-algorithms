(ns clojure-algorithms.data-structures.binary-tree.persistent-segment-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.persistent-segment-tree
             :refer [make-pst pst-update pst-query]]))

(deftest test-persistent-seg-tree
  (let [pst (make-pst [1 2 3 4])
        root (first (:roots pst))]
    (is (= 10 (:value root)))
    (is (= 3 (:value (:left root))))
    (is (= 7 (:value (:right root)))))
  (let [pst (make-pst [1 2 3 4])
        [pst v1] (pst-update pst 0 1 5)]
    (is (= 13 (pst-query pst v1 0 3)))
    (is (= 10 (pst-query pst 0 0 3)))
    (let [[pst v2] (pst-update pst v1 3 6)]
      (is (= 15 (pst-query pst v2 0 3)))
      (is (= 13 (pst-query pst v1 0 3)))))
  (let [pst (make-pst [1 2 3 4])]
    (is (= 10 (pst-query pst 0 0 3)))
    (is (= 5 (pst-query pst 0 1 2)))
    (is (= 4 (pst-query pst 0 3 3)))))
