(ns clojure-algorithms.sorts.tim-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.tim-sort :as ts]))

(deftest test-helpers
  (is (= 2 (ts/binary-search [1 3 5] 4 0 2)))
  (is (= 0 (ts/binary-search [1 3 5] 0 0 2)))
  (is (= 3 (ts/binary-search [1 3 5] 6 0 2)))
  (is (= [1 2 3] (ts/insertion-sort [3 2 1])))
  (is (= [1 2 3 4] (ts/merge [1 4] [2 3]))))

(deftest test-tim-sort
  (testing "doctest examples"
    (is (= [] (ts/tim-sort [])))
    (is (= [\P \h \n \o \t \y] (ts/tim-sort "Python")))
    (is (= [-1.1 -1 0 1 1.1] (ts/tim-sort [1.1 1 0 -1 -1.1])))
    (is (= [0 1 2 3 4 5 6] (ts/tim-sort [6 5 4 3 2 1 0])))
    (is (= [1 2 3] (ts/tim-sort [3 2 1]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (ts/tim-sort data))))))
