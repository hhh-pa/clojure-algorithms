(ns clojure-algorithms.sorts.bitonic-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.bitonic-sort :refer [bitonic-sort bitonic-merge
                                                           comp-and-swap]]))

(deftest test-comp-and-swap
  (is (= [12 -21 42 1] (comp-and-swap [12 42 -21 1] 1 2 1)))
  (is (= [12 42 -21 1] (comp-and-swap [12 -21 42 1] 1 2 0))))

(deftest test-bitonic-merge
  (is (= [-21 1 12 42] (bitonic-merge [12 42 -21 1] 0 4 1)))
  (is (= [42 12 1 -21] (bitonic-merge [-21 1 12 42] 0 4 0))))

(deftest test-bitonic-sort
  (testing "ascending and descending"
    (is (= [-167 -121 -23 0 12 34 92 145]
           (bitonic-sort [12 34 92 -23 0 -121 -167 145] 0 8 1)))
    (is (= [145 92 34 12 0 -23 -121 -167]
           (bitonic-sort [-167 -121 -23 0 12 34 92 145] 0 8 0))))
  (testing "strings and floats"
    (is (= ["apple" "banana" "cherry" "date"]
           (bitonic-sort ["banana" "apple" "cherry" "date"] 0 4 1)))
    (is (= [1.5 2 3 4.5] (bitonic-sort [3 1.5 2 4.5] 0 4 1))))
  (testing "single-arity sorts whole collection ascending"
    (is (= [1 2 3 4] (bitonic-sort [3 1 4 2])))))
