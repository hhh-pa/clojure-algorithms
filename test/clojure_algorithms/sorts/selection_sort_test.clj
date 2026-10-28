(ns clojure-algorithms.sorts.selection-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.selection-sort :refer [selection-sort]]))

(deftest test-selection-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (selection-sort [0 5 3 2 2])))
    (is (= [] (selection-sort [])))
    (is (= [-45 -5 -2] (selection-sort [-2 -5 -45])))
    (is (= [1] (selection-sort [1])))
    (is (= [1 2 3 4 5] (selection-sort [5 4 3 2 1])))
    (is (= [3 3 3 3] (selection-sort [3 3 3 3])))
    (is (= [-3 -1 0 2 5] (selection-sort [2 -3 0 5 -1])))
    (is (= ["a" "b" "c" "d"] (selection-sort ["d" "a" "c" "b"])))
    (is (= [0.5 1.1 2.4 3.2] (selection-sort [3.2 1.1 2.4 0.5]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 80 #(rand-int 100)))]
      (is (= (vec (sort data)) (selection-sort data))))))
