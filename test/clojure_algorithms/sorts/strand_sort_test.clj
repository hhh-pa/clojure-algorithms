(ns clojure-algorithms.sorts.strand-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.strand-sort :refer [strand-sort]]))

(deftest test-strand-sort
  (testing "doctest examples"
    (is (= [0 1 2 3 4 5] (strand-sort [4 2 5 3 0 1])))
    (is (= [5 4 3 2 1 0] (strand-sort [4 2 5 3 0 1] :reverse true)))
    (is (= ["apple" "banana" "cherry"]
           (strand-sort ["banana" "apple" "cherry"])))
    (is (= [1 2 3 4 5] (strand-sort [4 3 5 1 2])))
    (is (= [5 4 3 2 1] (strand-sort [4 3 5 1 2] :reverse true))))
  (testing "edges and random data"
    (is (= [] (strand-sort [])))
    (let [data (vec (repeatedly 50 #(rand-int 100)))]
      (is (= (vec (sort data)) (strand-sort data)))
      (is (= (vec (reverse (sort data))) (strand-sort data :reverse true))))))
