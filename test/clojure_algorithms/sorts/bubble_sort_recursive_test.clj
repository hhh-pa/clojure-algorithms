(ns clojure-algorithms.sorts.bubble-sort-recursive-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.bubble-sort-recursive :refer [bubble-sort-recursive]]))

(deftest test-bubble-sort-recursive
  (testing "doctest examples"
    (is (= [1 2 4 5 8] (bubble-sort-recursive [5 1 4 2 8])))
    (is (= [] (bubble-sort-recursive [])))
    (is (= [1] (bubble-sort-recursive [1])))
    (is (= [1 2 3 3] (bubble-sort-recursive [3 3 2 1])))
    (is (= [-2 -1 0 5] (bubble-sort-recursive [-1 5 0 -2])))
    (is (= ["apple" "banana" "cherry"]
           (bubble-sort-recursive ["banana" "apple" "cherry"])))
    (is (= [1.5 2.7 3.14] (bubble-sort-recursive [3.14 1.5 2.7]))))
  (testing "matches clojure sort on random data"
    (let [data (repeatedly 100 #(rand-int 200))]
      (is (= (vec (sort data)) (bubble-sort-recursive data))))))
