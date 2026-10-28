(ns clojure-algorithms.data-structures.binary-tree.segment-tree-other-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.segment-tree-other
             :refer [make-seg-tree seg-update query-range traverse node->str]]))

(deftest test-sum-seg-tree
  (let [st (make-seg-tree [2 1 5 3 4] +)]
    (is (= "SegmentTreeNode(start=0, end=4, val=15)"
           (node->str (first (traverse st)))))
    (let [st (seg-update st 1 5)]
      (is (= "SegmentTreeNode(start=0, end=4, val=19)"
             (node->str (first (traverse st)))))
      (is (= 12 (:val (second (traverse st)))))
      (is (= 7 (:val (nth (traverse st) 2))))
      (is (= 7 (query-range st 3 4)))
      (is (= 5 (query-range st 2 2)))
      (is (= 13 (query-range st 1 3))))))

(deftest test-max-seg-tree
  (let [st (make-seg-tree [2 1 5 3 4] max)]
    (is (= "SegmentTreeNode(start=0, end=4, val=5)"
           (node->str (first (traverse st)))))
    (let [st (seg-update st 1 5)]
      (is (= "SegmentTreeNode(start=0, end=4, val=5)"
             (node->str (first (traverse st)))))
      (is (= 4 (query-range st 3 4)))
      (is (= 5 (query-range st 2 2))))))
