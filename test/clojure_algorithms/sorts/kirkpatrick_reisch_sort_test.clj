(ns clojure-algorithms.sorts.kirkpatrick-reisch-sort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.kirkpatrick-reisch-sort :refer [kirkpatrick-reisch-sort]]))

(deftest test-kirkpatrick-reisch-sort
  (testing "doctest examples"
    (is (= [1 1 2 3 3 4 5 5 5 6 9]
           (kirkpatrick-reisch-sort [3 1 4 1 5 9 2 6 5 3 5])))
    (is (= [] (kirkpatrick-reisch-sort [])))
    (is (= [1] (kirkpatrick-reisch-sort [1])))
    (is (= [1 2 3 4 5] (kirkpatrick-reisch-sort [5 4 3 2 1])))
    (is (= [-3 -1 0 2 5] (kirkpatrick-reisch-sort [-1 -3 5 0 2]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 1000)))]
      (is (= (vec (sort data)) (kirkpatrick-reisch-sort data))))))
