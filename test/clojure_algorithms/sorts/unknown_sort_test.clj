(ns clojure-algorithms.sorts.unknown-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.unknown-sort :refer [merge-sort]]))

(deftest test-merge-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (merge-sort [0 5 3 2 2])))
    (is (= [] (merge-sort [])))
    (is (= [-45 -5 -2] (merge-sort [-2 -5 -45])))
    (is (= (vec (sort ["d" "a" "b" "e"])) (merge-sort ["d" "a" "b" "e"])))
    (is (= [1.1 2.2 3.3 4.4 5.5] (merge-sort [1.1 3.3 5.5 2.2 4.4]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 60 #(rand-int 100)))]
      (is (= (vec (sort data)) (merge-sort data))))))
