(ns clojure-algorithms.dynamic-programming.longest-increasing-subsequence-o-nlogn-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.longest-increasing-subsequence-o-nlogn :as lis]))

(deftest lis-length-test
  (testing "O(n log n) LIS length"
    (is (= 6 (lis/longest-increasing-subsequence-length [2 5 3 7 11 8 10 13 6])))
    (is (= 0 (lis/longest-increasing-subsequence-length [])))
    (is (= 6 (lis/longest-increasing-subsequence-length
              [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15])))
    (is (= 1 (lis/longest-increasing-subsequence-length [5 4 3 2 1])))
    (is (= 1 (lis/longest-increasing-subsequence-length [7])))))
