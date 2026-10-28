(ns clojure-algorithms.sorts.pigeonhole-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.pigeonhole-sort :refer [pigeonhole-sort]]))

(deftest test-pigeonhole-sort
  (testing "doctest example"
    (is (= (vec (sort [8 3 2 7 4 6 8]))
           (pigeonhole-sort [8 3 2 7 4 6 8])))
    (is (= [] (pigeonhole-sort []))))
  (testing "negatives"
    (is (= [-45 -5 -2] (pigeonhole-sort [-2 -5 -45]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(- (rand-int 100) 50)))]
      (is (= (vec (sort data)) (pigeonhole-sort data))))))
