(ns clojure-algorithms.data-structures.binary-tree.segment-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.segment-tree
             :refer [left-index right-index make-seg-tree seg-update seg-query
                     show-data]]))

(deftest test-segment-tree
  (is (= 2 (left-index 1)))
  (is (= 4 (left-index 2)))
  (is (= 3 (right-index 1)))
  (is (= 5 (right-index 2)))
  (let [[st ok] (seg-update (make-seg-tree [1 2 3 4 5]) 2 4 10)]
    (is (true? ok))
    (is (= 10 (seg-query st 1 5))))
  (let [st (make-seg-tree [1 2 3 4 5])]
    (is (= 3 (seg-query st 1 3)))
    (is (= 5 (seg-query st 1 5))))
  (let [st (make-seg-tree [1 2 -4 7 3 -5 6 11 -20 9 14 15 5 2 -8])]
    (is (= 7 (seg-query st 4 6)))
    (is (= 14 (seg-query st 7 11)))
    (is (= 15 (seg-query st 7 12)))
    (let [[st _] (seg-update st 1 3 111)]
      (is (= 111 (seg-query st 1 15)))
      (let [[st _] (seg-update st 7 8 235)]
        (is (= [111 111 111 7 3 -5 235 235 -20 9 14 15 5 2 -8]
               (show-data st)))))))
