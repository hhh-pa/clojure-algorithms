(ns clojure-algorithms.sorts.wiggle-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.wiggle-sort :refer [wiggle-sort wiggle?]]))

(deftest test-wiggle-sort
  (testing "doctest examples"
    (is (= [0 5 2 3 2] (wiggle-sort [0 5 3 2 2])))
    (is (= [] (wiggle-sort [])))
    (is (= [-5 -2 -45] (wiggle-sort [-2 -5 -45])))
    (is (= [-5.68 -2.1 -45.11] (wiggle-sort [-2.1 -5.68 -45.11]))))
  (testing "wiggle property holds on random data"
    (doseq [_ (range 10)]
      (let [data (vec (repeatedly 20 #(rand-int 100)))]
        (is (wiggle? (wiggle-sort data)) (str data))))))
