(ns clojure-algorithms.sorts.circle-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.circle-sort :refer [circle-sort]]))

(deftest test-circle-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (circle-sort [0 5 3 2 2])))
    (is (= [] (circle-sort [])))
    (is (= [-45 -2 0 5] (circle-sort [-2 5 0 -45])))
    (is (= ["a" "b" "c" "d"] (circle-sort ["d" "a" "c" "b"])))
    (is (= [-1.0 0.0 2.5] (circle-sort [2.5 -1.0 0.0]))))
  (testing "matches clojure sort on random data"
    (doseq [_ (range 5)]
      (let [data (vec (repeatedly 30 #(rand-int 100)))]
        (is (= (vec (sort data)) (circle-sort data)))))))
