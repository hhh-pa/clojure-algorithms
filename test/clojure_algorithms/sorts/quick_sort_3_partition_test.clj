(ns clojure-algorithms.sorts.quick-sort-3-partition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.quick-sort-3-partition
             :refer [quick-sort-3partition quick-sort-lomuto-partition
                     lomuto-partition hoare-partition-by-value
                     hoare-partition-by-pivot quicksort-hoare
                     three-way-radix-quicksort]]))

(deftest test-3partition
  (is (= [-1 -1 0 5 5 5 24]
         (quick-sort-3partition [5 -1 -1 5 5 24 0] 0 6)))
  (is (= [0 2 6 9] (quick-sort-3partition [9 0 2 6] 0 3)))
  (is (= [] (quick-sort-3partition [] 0 0)))
  (is (= [1 2 3] (quick-sort-3partition [3 1 2]))))

(deftest test-lomuto
  (is (= [0 1 2 3 5] (quick-sort-lomuto-partition [0 5 3 1 2] 0 4)))
  (is (= [] (quick-sort-lomuto-partition [] 0 0)))
  (is (= [-4 -2 0 5] (quick-sort-lomuto-partition [-2 5 0 -4] 0 3)))
  (is (= 2 (second (lomuto-partition [1 5 7 6] 0 3)))))

(deftest test-hoare
  (let [[a idx] (hoare-partition-by-value [7 3 5 4 1 8 6] 5)]
    (is (= 3 idx))
    (is (= [1 3 4 5 7 8 6] a)))
  (is (= 0 (second (hoare-partition-by-value [7 3 5 4 1 8 6] 0))))
  (is (= 7 (second (hoare-partition-by-value [7 3 5 4 1 8 6] 9))))
  (let [[a idx] (hoare-partition-by-pivot [7 3 5 4 1 8 6] 3)]
    (is (= 2 idx))
    (is (= [1 3 4 6 7 8 5] a)))
  (is (= [0 1 2 2 2 3 7 8 8 8]
         (quicksort-hoare [2 2 8 0 3 7 2 1 8 8]))))

(deftest test-three-way-radix
  (is (= [] (three-way-radix-quicksort [])))
  (is (= [1] (three-way-radix-quicksort [1])))
  (is (= [-5 -2 -2 0 1 1]
         (three-way-radix-quicksort [-5 -2 1 -2 0 1])))
  (is (= [-1 0 0 1 1 2 2 2 5 5]
         (three-way-radix-quicksort [1 2 5 1 2 0 0 5 2 -1]))))

(deftest test-random
  (let [data (vec (repeatedly 80 #(rand-int 60)))]
    (is (= (vec (sort data)) (quick-sort-3partition data)))
    (is (= (vec (sort data)) (quick-sort-lomuto-partition data)))
    (is (= (vec (sort data)) (quicksort-hoare data)))
    (is (= (vec (sort data)) (three-way-radix-quicksort data)))))
