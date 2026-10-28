(ns clojure-algorithms.sorts.counting-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.counting-sort :refer [counting-sort
                                                            counting-sort-string]]))

(deftest test-counting-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (counting-sort [0 5 3 2 2])))
    (is (= [] (counting-sort [])))
    (is (= [-45 -5 -2] (counting-sort [-2 -5 -45]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 200 #(- (rand-int 200) 100)))]
      (is (= (vec (sort data)) (counting-sort data))))))

(deftest test-counting-sort-string
  (is (= "eghhiiinrsssttt" (counting-sort-string "thisisthestring")))
  (is (= "" (counting-sort-string ""))))
