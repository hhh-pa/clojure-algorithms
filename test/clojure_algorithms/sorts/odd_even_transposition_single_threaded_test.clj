(ns clojure-algorithms.sorts.odd-even-transposition-single-threaded-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.odd-even-transposition-single-threaded
             :refer [odd-even-transposition]]))

(deftest test-odd-even-transposition
  (testing "doctest examples"
    (is (= [1 2 3 4 5] (odd-even-transposition [5 4 3 2 1])))
    (is (= (vec (sort [13 11 18 0 -1]))
           (odd-even-transposition [13 11 18 0 -1])))
    (is (= (vec (sort [-0.1 1.1 0.1 -2.9]))
           (odd-even-transposition [-0.1 1.1 0.1 -2.9])))
    (is (= [] (odd-even-transposition [])))
    (is (= [1 1 2 2 3 3] (odd-even-transposition [3 3 1 2 2 1])))
    (is (= ["a" "b" "c"] (odd-even-transposition ["c" "a" "b"])))
    (is (= [1 2 4 7] (odd-even-transposition [4 2 7 1]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (odd-even-transposition data))))))
