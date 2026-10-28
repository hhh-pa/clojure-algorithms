(ns clojure-algorithms.sorts.msd-radix-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.msd-radix-sort :refer [msd-radix-sort
                                                             msd-radix-sort-inplace]]))

(deftest test-msd-radix-sort
  (testing "doctest examples"
    (is (= [1 4 12 40 100] (msd-radix-sort [40 12 1 100 4])))
    (is (= [] (msd-radix-sort [])))
    (is (= [80 123 123 345] (msd-radix-sort [123 345 123 80])))
    (is (= [1 45 1209 540402 834598]
           (msd-radix-sort [1209 834598 1 540402 45]))))
  (testing "negatives throw"
    (is (thrown? IllegalArgumentException (msd-radix-sort [-1 34 45]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 150 #(rand-int 100000)))]
      (is (= (vec (sort data)) (msd-radix-sort data))))))

(deftest test-msd-radix-sort-inplace
  (is (= (vec (sort [1 345 23 89 0 3]))
         (msd-radix-sort-inplace [1 345 23 89 0 3])))
  (is (= [0 0 0 1 3 3 24 43]
         (msd-radix-sort-inplace [1 43 0 0 0 24 3 3])))
  (is (= [] (msd-radix-sort-inplace [])))
  (is (thrown? IllegalArgumentException
               (msd-radix-sort-inplace [-1 34 23 4 -42]))))
