(ns clojure-algorithms.sorts.pigeon-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.pigeon-sort :refer [pigeon-sort]]))

(deftest test-pigeon-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (pigeon-sort [0 5 3 2 2])))
    (is (= [] (pigeon-sort [])))
    (is (= [-45 -5 -2] (pigeon-sort [-2 -5 -45]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(- (rand-int 100) 50)))]
      (is (= (vec (sort data)) (pigeon-sort data))))))
