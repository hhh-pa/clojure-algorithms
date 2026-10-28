(ns clojure-algorithms.sorts.binary-insertion-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.binary-insertion-sort :refer [binary-insertion-sort]]))

(deftest test-binary-insertion-sort
  (testing "doctest examples"
    (is (= [0 1 4 4 1234] (binary-insertion-sort [0 4 1234 4 1])))
    (is (= [] (binary-insertion-sort [])))
    (is (= [-3 -2 -1] (binary-insertion-sort [-1 -2 -3])))
    (is (= (vec (sort ["d" "a" "b" "e" "c"]))
           (binary-insertion-sort ["d" "a" "b" "e" "c"]))))
  (testing "matches clojure sort on random data"
    (let [data (repeatedly 200 #(rand-int 200))]
      (is (= (vec (sort data)) (binary-insertion-sort data))))))
