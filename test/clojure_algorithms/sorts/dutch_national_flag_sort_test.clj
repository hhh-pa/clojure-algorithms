(ns clojure-algorithms.sorts.dutch-national-flag-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.dutch-national-flag-sort :refer [dutch-national-flag-sort]]))

(deftest test-dutch-national-flag-sort
  (testing "doctest examples"
    (is (= [] (dutch-national-flag-sort [])))
    (is (= [0] (dutch-national-flag-sort [0])))
    (is (= [0 0 1 1 2 2]
           (dutch-national-flag-sort [2 1 0 0 1 2])))
    (is (= [0 0 0 0 0 1 1 1 1 1 2 2]
           (dutch-national-flag-sort [0 1 1 0 1 2 1 2 0 0 0 1]))))
  (testing "invalid input"
    (is (thrown? IllegalArgumentException
                 (dutch-national-flag-sort [3 2 3 1 3 0 3])))
    (is (thrown? IllegalArgumentException
                 (dutch-national-flag-sort [-1 2 -1 1 -1 0 -1]))))
  (testing "random ternary data"
    (let [data (vec (repeatedly 100 #(rand-int 3)))]
      (is (= (vec (sort data)) (dutch-national-flag-sort data))))))
