(ns clojure-algorithms.data-structures.binary-tree.wavelet-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.wavelet-tree
             :refer [test-array make-node node->str build-tree rank-till-index
                     wavelet-rank quantile range-counting]]))

(deftest test-wavelet-basics
  (is (= "Node(min_value=-1 max_value=-1)" (node->str (make-node 27))))
  (is (= "Node(min_value=0 max_value=9)"
         (node->str (build-tree test-array)))))

(deftest test-rank
  (let [root (build-tree test-array)]
    (is (= 1 (rank-till-index root 6 6)))
    (is (= 1 (rank-till-index root 2 0)))
    (is (= 2 (rank-till-index root 1 10)))
    (is (= 0 (rank-till-index root 17 7)))
    (is (= 1 (rank-till-index root 0 9)))
    (is (= 2 (wavelet-rank root 6 3 13)))
    (is (= 4 (wavelet-rank root 2 0 19)))
    (is (= 0 (wavelet-rank root 9 2 2)))
    (is (= 2 (wavelet-rank root 0 5 10)))))

(deftest test-quantile-range
  (let [root (build-tree test-array)]
    (is (= 5 (quantile root 2 2 5)))
    (is (= 4 (quantile root 5 2 13)))
    (is (= 8 (quantile root 0 6 6)))
    (is (= -1 (quantile root 4 2 5)))
    (is (= 3 (range-counting root 1 10 3 7)))
    (is (= 1 (range-counting root 2 2 1 4)))
    (is (= 20 (range-counting root 0 19 0 100)))
    (is (= 0 (range-counting root 1 0 1 100)))
    (is (= 0 (range-counting root 0 17 100 1)))))
