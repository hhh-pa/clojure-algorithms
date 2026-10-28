(ns clojure-algorithms.data-structures.hashing.double-hash-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.double-hash
             :refer [make-double-hash ht-keys insert-data]]))

(deftest test-double-hash
  (is (= {1 10 2 20 0 30}
         (ht-keys (reduce insert-data (make-double-hash 3) [10 20 30]))))
  (is (= {10 10 9 20 8 30}
         (ht-keys (reduce insert-data (make-double-hash 2) [10 20 30]))))
  (is (= {9 20 10 10 8 30}
         (ht-keys (reduce insert-data (make-double-hash 4) [10 20 30])))))
