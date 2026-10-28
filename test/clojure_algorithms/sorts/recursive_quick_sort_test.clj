(ns clojure-algorithms.sorts.recursive-quick-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.recursive-quick-sort :refer [quick-sort]]))

(deftest test-quick-sort
  (testing "doctest examples"
    (is (= [0 1 2] (quick-sort [2 1 0])))
    (is (= [0 1.1 2.2] (quick-sort [2.2 1.1 0])))
    (is (= ["a" "b" "c"] (quick-sort ["c" "a" "b"])))
    (is (= [] (quick-sort [])))
    (is (= [0] (quick-sort [0])))
    (is (= (vec (sort [2.5 -1 0.0])) (quick-sort [2.5 -1 0.0])))
    (is (= (vec (sort (seq "quick_sort")))
           (quick-sort (seq "quick_sort")))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (quick-sort data))))))
