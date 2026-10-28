(ns clojure-algorithms.sorts.heap-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.heap-sort :refer [heap-sort heapify]]))

(deftest test-heapify
  (is (= [4 5 3 1 2] (heapify [1 4 3 5 2] 0 5)))
  (is (= [5 4 3 1 2] (heapify [4 5 3 1 2] 0 5))))

(deftest test-heap-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (heap-sort [0 5 3 2 2])))
    (is (= [] (heap-sort [])))
    (is (= [-45 -5 -2] (heap-sort [-2 -5 -45])))
    (is (= [-200 -30 -5 0 3 4 7 8 9 28 123]
           (heap-sort [3 7 9 28 123 -5 8 -30 -200 0 4])))
    (is (= ["apple" "banana" "cherry"]
           (heap-sort ["banana" "apple" "cherry"])))
    (is (= [1.5 2.7 3.14] (heap-sort [3.14 1.5 2.7]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 150 #(rand-int 300)))]
      (is (= (vec (sort data)) (heap-sort data))))))
