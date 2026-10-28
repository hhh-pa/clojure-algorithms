(ns clojure-algorithms.sorts.comb-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.comb-sort :refer [comb-sort]]))

(deftest test-comb-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (comb-sort [0 5 3 2 2])))
    (is (= [] (comb-sort [])))
    (is (= [-15 -7 0 2 3 8 45 99]
           (comb-sort [99 45 -7 8 2 0 -15 3])))
    (is (= [0 1 2 3 4 5 6] (comb-sort [2 0 3 4 5 6 1])))
    (is (= ["a" "b" "c"] (comb-sort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (comb-sort [2.5 -1 0.0]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 150 #(rand-int 300)))]
      (is (= (vec (sort data)) (comb-sort data))))))
