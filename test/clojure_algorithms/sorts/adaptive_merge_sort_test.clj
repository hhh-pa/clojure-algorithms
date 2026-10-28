(ns clojure-algorithms.sorts.adaptive-merge-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.adaptive-merge-sort :refer [adaptive-merge-sort]]))

(deftest test-adaptive-merge-sort
  (testing "basic cases"
    (is (= [] (adaptive-merge-sort []))))
  (testing "already sorted input skips merging"
    (is (= [1 2 3 4 5] (adaptive-merge-sort [1 2 3 4 5]))))
  (testing "reverse sorted"
    (is (= [1 2 3 4 5] (adaptive-merge-sort [5 4 3 2 1]))))
  (testing "duplicates and negatives"
    (is (= [-45 -5 -2 0 3] (adaptive-merge-sort [0 -5 3 -45 -2]))))
  (testing "matches clojure sort on random data"
    (let [data (repeatedly 200 #(rand-int 1000))]
      (is (= (vec (sort data)) (adaptive-merge-sort data)))))
  (testing "strings"
    (is (= ["a" "b" "c"] (adaptive-merge-sort ["c" "a" "b"])))))
