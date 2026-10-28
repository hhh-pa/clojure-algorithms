(ns clojure-algorithms.sorts.radix-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.radix-sort :refer [radix-sort]]))

(deftest test-radix-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (radix-sort [0 5 3 2 2])))
    (is (= (vec (range 15)) (radix-sort (vec (range 15)))))
    (is (= (vec (range 15)) (radix-sort (vec (range 14 -1 -1)))))
    (is (= [1 10 100 1000] (radix-sort [1 100 10 1000]))))
  (testing "negatives throw"
    (is (thrown? IllegalArgumentException (radix-sort [-1 2 3]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 150 #(rand-int 100000)))]
      (is (= (vec (sort data)) (radix-sort data))))))
