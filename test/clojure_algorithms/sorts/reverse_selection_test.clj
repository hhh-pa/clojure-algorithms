(ns clojure-algorithms.sorts.reverse-selection-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.reverse-selection :refer [reverse-subarray
                                                                reverse-selection-sort]]))

(deftest test-reverse-subarray
  (is (= [1 4 3 2 5] (reverse-subarray [1 2 3 4 5] 1 3)))
  (is (= [1] (reverse-subarray [1] 0 0)))
  (is (= [2 1] (reverse-subarray [1 2] 0 1))))

(deftest test-reverse-selection-sort
  (testing "doctest examples"
    (is (= [1 5 6 9 17 21]
           (reverse-selection-sort [1 9 5 21 17 6])))
    (is (= [] (reverse-selection-sort [])))
    (is (= [-48 -17 -3] (reverse-selection-sort [-3 -17 -48])))
    (is (= [1 1 1 1] (reverse-selection-sort [1 1 1 1])))
    (is (= [1 2 3 4 5] (reverse-selection-sort [5 4 3 2 1])))
    (is (= ["apple" "banana" "cherry"]
           (reverse-selection-sort ["banana" "apple" "cherry"])))
    (is (= [1.5 2.7 3.14] (reverse-selection-sort [3.14 1.5 2.7]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 60 #(rand-int 100)))]
      (is (= (vec (sort data)) (reverse-selection-sort data))))))
