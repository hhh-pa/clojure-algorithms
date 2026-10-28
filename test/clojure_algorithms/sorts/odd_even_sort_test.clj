(ns clojure-algorithms.sorts.odd-even-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.odd-even-sort :refer [odd-even-sort]]))

(deftest test-odd-even-sort
  (testing "doctest examples"
    (is (= [1 2 3 4 5] (odd-even-sort [5 4 3 2 1])))
    (is (= [] (odd-even-sort [])))
    (is (= [-10 -1 2 10] (odd-even-sort [-10 -1 10 2])))
    (is (= [1 2 3 4] (odd-even-sort [1 2 3 4])))
    (is (= ["a" "b" "c"] (odd-even-sort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (odd-even-sort [2.5 -1 0.0]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 80 #(rand-int 100)))]
      (is (= (vec (sort data)) (odd-even-sort data))))))
