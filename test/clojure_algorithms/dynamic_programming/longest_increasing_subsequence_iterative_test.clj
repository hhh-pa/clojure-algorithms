(ns clojure-algorithms.dynamic-programming.longest-increasing-subsequence-iterative-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.longest-increasing-subsequence-iterative :as lis]))

(deftest longest-subsequence-test
  (testing "iterative longest non-decreasing subsequence"
    (is (= [10 22 33 50 60 80]
           (lis/longest-subsequence [10 22 9 33 21 50 41 60 80])))
    (is (= [1 2 3 9]
           (lis/longest-subsequence [4 8 7 5 1 12 2 3 9])))
    (is (= [7 7] (lis/longest-subsequence [9 8 7 6 5 7])))
    (is (= [12 23 35 39] (lis/longest-subsequence [28 26 12 23 35 39])))
    (is (= [1 1 1] (lis/longest-subsequence [1 1 1])))
    (is (= [] (lis/longest-subsequence [])))))
