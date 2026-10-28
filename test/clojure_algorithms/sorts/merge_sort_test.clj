(ns clojure-algorithms.sorts.merge-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.merge-sort :refer [merge-sort]]))

(deftest test-merge-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (merge-sort [0 5 3 2 2])))
    (is (= [] (merge-sort [])))
    (is (= [-45 -5 -2] (merge-sort [-2 -45 -5]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 200 #(rand-int 500)))]
      (is (= (vec (sort data)) (merge-sort data))))))
