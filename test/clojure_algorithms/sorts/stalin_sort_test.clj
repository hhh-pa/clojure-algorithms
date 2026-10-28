(ns clojure-algorithms.sorts.stalin-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.stalin-sort :refer [stalin-sort]]))

(deftest test-stalin-sort
  (testing "doctest examples"
    (is (= [4 5 7] (stalin-sort [4 3 5 2 1 7])))
    (is (= [1 2 3 4] (stalin-sort [1 2 3 4])))
    (is (= [4 5 5] (stalin-sort [4 5 5 2 3])))
    (is (= [6 11 12] (stalin-sort [6 11 12 4 1 5])))
    (is (= [5] (stalin-sort [5 0 4 3])))
    (is (= [5] (stalin-sort [5 4 3 2 1])))
    (is (= [1 2 3 4 5] (stalin-sort [1 2 3 4 5])))
    (is (= [1 2 8] (stalin-sort [1 2 8 7 6])))
    (is (= [] (stalin-sort [])))
    (is (= [7] (stalin-sort [7])))))
