(ns clojure-algorithms.sorts.exchange-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.exchange-sort :refer [exchange-sort]]))

(deftest test-exchange-sort
  (testing "doctest examples"
    (is (= [1 2 3 4 5] (exchange-sort [5 4 3 2 1])))
    (is (= [-3 -2 -1] (exchange-sort [-1 -2 -3])))
    (is (= [1 2 3 4 5] (exchange-sort [1 2 3 4 5])))
    (is (= [-2 0 3 5 10] (exchange-sort [0 10 -2 5 3])))
    (is (= [] (exchange-sort [])))
    (is (= ["a" "b" "c"] (exchange-sort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (exchange-sort [2.5 -1 0.0]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 80 #(rand-int 100)))]
      (is (= (vec (sort data)) (exchange-sort data))))))
