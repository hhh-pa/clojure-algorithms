(ns clojure-algorithms.data-structures.hashing.hash-table-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.hash-table
             :refer [make-hash-table ht-keys balanced-factor hash-function
                     insert-data bulk-insert]]))

(deftest test-insert
  (let [ht (reduce insert-data (make-hash-table 10) [10 20 30])]
    (is (= {0 10 1 20 2 30} (ht-keys ht))))
  (let [ht (reduce insert-data (make-hash-table 5) [5 4 3 2 1])]
    (is (= {0 5 4 4 3 3 2 2 1 1} (ht-keys ht)))))

(deftest test-hash-function
  (let [ht (make-hash-table 5)]
    (is (= 0 (hash-function ht 10)))
    (is (= 0 (hash-function ht 20)))
    (is (= 4 (hash-function ht 4)))
    (is (= 3 (hash-function ht 18)))
    (is (= 2 (hash-function ht -18)))
    (is (= 3.5 (hash-function ht 18.5)))
    (is (= 0 (hash-function ht 0)))))

(deftest test-rehashing
  (let [ht (reduce insert-data (make-hash-table 3) (range 20))]
    (is (= 20 (count (ht-keys ht))))
    (doseq [v (range 20)]
      (is (contains? (set (vals (ht-keys ht))) v)))))

(deftest test-bulk-insert
  (let [ht (bulk-insert (make-hash-table 5) [10 20 30])]
    (is (= 3 (count (ht-keys ht))))))
