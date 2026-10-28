(ns clojure-algorithms.sorts.quick-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.quick-sort :refer [quick-sort]]))

(deftest test-quick-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (quick-sort [0 5 3 2 2])))
    (is (= [] (quick-sort [])))
    (is (= [-45 -2 0 5] (quick-sort [-2 5 0 -45])))
    (is (= ["a" "b" "m" "z"] (quick-sort ["z" "a" "m" "b"])))
    (is (= [-1.0 2.71 3.14] (quick-sort [3.14 -1.0 2.71]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 150 #(rand-int 300)))]
      (is (= (vec (sort data)) (quick-sort data))))))
