(ns clojure-algorithms.divide-and-conquer.mergesort-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.mergesort :as ms]))

(deftest merge-halves-test
  (testing "merge helper"
    (is (= [-2 -1] (ms/merge-halves [-2] [-1])))
    (is (= [1 2 3 4 5 6] (ms/merge-halves [1 2 3] [4 5 6])))
    (is (= [12 13 14 15] (ms/merge-halves [12 15] [13 14])))
    (is (= [] (ms/merge-halves [] [])))))

(deftest merge-sort-test
  (testing "merge sort"
    (is (= [-200 -10 -2 3 11 99 100 100000]
           (ms/merge-sort (shuffle [-2 3 -10 11 99 100000 100 -200]))))
    (is (= [-200] (ms/merge-sort [-200])))
    (is (= [-2] (ms/merge-sort [-2])))
    (is (= [] (ms/merge-sort [])))
    (let [arr [10000000 1 -1111111111 101111111112 9000002]]
      (is (= (sort arr) (ms/merge-sort arr))))))
