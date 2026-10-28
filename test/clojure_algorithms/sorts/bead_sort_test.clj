(ns clojure-algorithms.sorts.bead-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.bead-sort :refer [bead-sort]]))

(deftest test-bead-sort
  (testing "doctest examples"
    (is (= [1 4 5 6 11 12] (bead-sort [6 11 12 4 1 5])))
    (is (= [1 2 3 4 5 6 7 8 9] (bead-sort [9 8 7 6 5 4 3 2 1])))
    (is (= [0 3 4 5] (bead-sort [5 0 4 3])))
    (is (= [1 2 8] (bead-sort [8 2 1]))))
  (testing "edges"
    (is (= [] (bead-sort [])))
    (is (= [5] (bead-sort [5])))
    (is (= [0 0 0] (bead-sort [0 0 0]))))
  (testing "invalid input"
    (is (thrown? IllegalArgumentException (bead-sort [1 0.9 0.0 0 -1 -0.9])))
    (is (thrown? IllegalArgumentException (bead-sort [-1 2 3])))))
