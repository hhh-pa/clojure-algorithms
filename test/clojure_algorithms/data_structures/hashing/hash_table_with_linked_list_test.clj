(ns clojure-algorithms.data-structures.hashing.hash-table-with-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.hash-table-with-linked-list
             :refer [make-hash-table ht-keys insert-data]]))

(deftest test-chained-table
  (let [ht (reduce insert-data (make-hash-table 10) [10 20 30])]
    (is (= {0 [30 20 10]} (ht-keys ht))))
  (let [ht (reduce insert-data (make-hash-table 10) [10 21 32])]
    (is (= {0 [10] 1 [21] 2 [32]} (ht-keys ht))))
  (let [ht (reduce insert-data (make-hash-table 5) [5 10 15])]
    (is (= {0 [15 10 5]} (ht-keys ht))))
  (let [ht (reduce insert-data (make-hash-table 3) (range 12))]
    (is (= (set (range 12)) (set (mapcat identity (vals (ht-keys ht))))))))
