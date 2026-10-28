(ns clojure-algorithms.sorts.double-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.double-sort :refer [double-sort]]))

(deftest test-double-sort
  (testing "doctest examples"
    (is (= [-7 -6 -5 -4 -3 -2 -1]
           (double-sort [-1 -2 -3 -4 -5 -6 -7])))
    (is (= [] (double-sort [])))
    (is (= [-6 -5 -4 -3 -2 -1] (double-sort [-1 -2 -3 -4 -5 -6])))
    (is (= (vec (sort [-3 10 16 -42 29]))
           (double-sort [-3 10 16 -42 29])))
    (is (= ["a" "b" "c"] (double-sort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (double-sort [2.5 -1 0.0]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 60 #(rand-int 100)))]
      (is (= (vec (sort data)) (double-sort data))))))
