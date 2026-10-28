(ns clojure-algorithms.sorts.tree-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.tree-sort :refer [tree-sort]]))

(deftest test-tree-sort
  (testing "doctest examples"
    (is (= [] (tree-sort [])))
    (is (= [1] (tree-sort [1])))
    (is (= [1 2] (tree-sort [1 2])))
    (is (= [2 5 7] (tree-sort [5 2 7])))
    (is (= [-4 2 5 7 9] (tree-sort [5 -4 9 2 7])))
    (is (= [-1 1 2 4 5 6 7 37] (tree-sort [5 6 1 -1 4 37 2 7])))
    (is (= (vec (sort (range 10 -10 -1))) (tree-sort (range 10 -10 -1))))
    (is (= ["a" "b" "c"] (tree-sort ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (tree-sort [2.5 -1 0.0])))
    (is (= [1 1 2 3 3] (tree-sort [3 1 3 2 1])))
    (is (= [2 2 2] (tree-sort [2 2 2]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (tree-sort data))))))
