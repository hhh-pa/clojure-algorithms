(ns clojure-algorithms.sorts.patience-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.patience-sort :refer [patience-sort]]))

(deftest test-patience-sort
  (testing "doctest examples"
    (is (= [1 5 6 9 17 21] (patience-sort [1 9 5 21 17 6])))
    (is (= [] (patience-sort [])))
    (is (= [-48 -17 -3] (patience-sort [-3 -17 -48])))
    (is (= (vec (sort ["d" "a" "b" "e"]))
           (patience-sort ["d" "a" "b" "e"])))
    (is (= [1.1 2.2 3.3] (patience-sort [3.3 1.1 2.2]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (patience-sort data))))))
