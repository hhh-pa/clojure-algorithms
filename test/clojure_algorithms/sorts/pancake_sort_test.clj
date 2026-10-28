(ns clojure-algorithms.sorts.pancake-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.pancake-sort :refer [pancake-sort]]))

(deftest test-pancake-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (pancake-sort [0 5 3 2 2])))
    (is (= [] (pancake-sort [])))
    (is (= [-45 -5 -2] (pancake-sort [-2 -5 -45])))
    (is (= (vec (sort ["d" "a" "b" "e" "c"]))
           (pancake-sort ["d" "a" "b" "e" "c"]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 60 #(rand-int 100)))]
      (is (= (vec (sort data)) (pancake-sort data))))))
