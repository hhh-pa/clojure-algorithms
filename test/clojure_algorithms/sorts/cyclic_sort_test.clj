(ns clojure-algorithms.sorts.cyclic-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.cyclic-sort :refer [cyclic-sort]]))

(deftest test-cyclic-sort
  (testing "doctest examples"
    (is (= [] (cyclic-sort [])))
    (is (= [1 2 3 4 5] (cyclic-sort [3 5 2 1 4]))))
  (testing "shuffled range"
    (is (= [1 2 3 4 5 6 7 8] (cyclic-sort [4 2 8 6 1 7 3 5]))))
  (testing "invalid input"
    (is (thrown? IllegalArgumentException (cyclic-sort [1 2 2])))
    (is (thrown? IllegalArgumentException (cyclic-sort [1 5])))))
