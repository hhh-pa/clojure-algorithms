(ns clojure-algorithms.sorts.flash-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.flash-sort :refer [flash-sort]]))

(deftest test-flash-sort
  (testing "doctest examples"
    (is (= [1 2 3 4 7 9] (flash-sort [4 2 7 1 9 3])))
    (is (= [] (flash-sort [])))
    (is (= [5] (flash-sort [5])))
    (is (= [3 3 3 3] (flash-sort [3 3 3 3])))
    (is (= [-5 -1 0 2 3] (flash-sort [-1 -5 0 3 2])))
    (is (= [0.1 1.2 1.5 2.3 3.7]
           (flash-sort [1.5 2.3 0.1 3.7 1.2])))
    (is (= [1 2 3 4 5 6 7 8 9 10]
           (flash-sort [10 9 8 7 6 5 4 3 2 1])))
    (is (= [42] (flash-sort [42])))
    (is (= [1.1 1.1 2.5 2.5 3.3]
           (flash-sort [2.5 1.1 3.3 2.5 1.1])))
    (is (= [4 4 6 6 6] (flash-sort [6 6 4 4 6])))
    (is (= [3 6 8 8 8] (flash-sort [8 3 8 6 8]))))
  (testing "matches clojure sort on random data"
    (doseq [_ (range 5)]
      (let [data (vec (repeatedly 60 #(rand-int 200)))]
        (is (= (vec (sort data)) (flash-sort data)))))
    (let [data (vec (repeatedly 60 #(- (rand-int 200) 100)))]
      (is (= (vec (sort data)) (flash-sort data))))))
