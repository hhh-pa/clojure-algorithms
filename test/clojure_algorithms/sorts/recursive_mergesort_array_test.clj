(ns clojure-algorithms.sorts.recursive-mergesort-array-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.sorts.recursive-mergesort-array :as rms]))

(deftest test-merge
  (testing "doctest examples"
    (is (= [1 2 3 4 5 6 7 8 9 10]
           (rms/merge [10 9 8 7 6 5 4 3 2 1])))
    (is (= [1 2 3 4 5 6 7 8 9 10]
           (rms/merge [1 2 3 4 5 6 7 8 9 10])))
    (is (= [1 2 3 9 10 15 22 23]
           (rms/merge [10 22 1 2 3 9 15 23])))
    (is (= [100] (rms/merge [100])))
    (is (= [] (rms/merge [])))
    (is (= ["a" "b" "c"] (rms/merge ["c" "a" "b"])))
    (is (= [-1 0.0 2.5] (rms/merge [2.5 -1 0.0])))
    (is (= [1 2 3] (rms/merge [3 1 2]))))
  (testing "matches clojure sort on random data"
    (let [data (vec (repeatedly 100 #(rand-int 200)))]
      (is (= (vec (sort data)) (rms/merge data))))))
