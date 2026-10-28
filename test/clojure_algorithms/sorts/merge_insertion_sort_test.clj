(ns clojure-algorithms.sorts.merge-insertion-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.merge-insertion-sort :refer [merge-insertion-sort
                                                                   binary-search-insertion
                                                                   merge-pairs
                                                                   sortlist-2d]]))

(deftest test-helpers
  (is (= [1 2 4 7 9 10]
         (binary-search-insertion [1 2 7 9 10] 4)))
  (is (= [[1 6] [2 3] [4 5] [7 8] [9 10]]
         (merge-pairs [[1 6] [9 10]] [[2 3] [4 5] [7 8]])))
  (is (= [[1 6] [2 3] [4 5] [7 8] [9 10]]
         (sortlist-2d [[9 10] [1 6] [7 8] [2 3] [4 5]]))))

(defn- permutations
  [xs]
  (if (empty? xs)
    [[]]
    (for [i (range (count xs))
          rest-p (permutations (into (subvec (vec xs) 0 i)
                                     (subvec (vec xs) (inc i))))]
      (cons (nth xs i) rest-p))))

(deftest test-merge-insertion-sort
  (testing "doctest examples"
    (is (= [0 2 2 3 5] (merge-insertion-sort [0 5 3 2 2])))
    (is (= [99] (merge-insertion-sort [99])))
    (is (= [-45 -5 -2] (merge-insertion-sort [-2 -5 -45]))))
  (testing "all permutations of range 5"
    (is (every? #(= [0 1 2 3 4] (merge-insertion-sort %))
                (permutations [0 1 2 3 4]))))
  (testing "random data"
    (let [data (vec (repeatedly 40 #(rand-int 100)))]
      (is (= (vec (sort data)) (merge-insertion-sort data))))))
