(ns clojure-algorithms.dynamic-programming.longest-palindromic-subsequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.longest-palindromic-subsequence :as lps]))

(deftest lps-test
  (testing "longest palindromic subsequence lengths"
    (is (= 4 (lps/longest-palindromic-subsequence "bbbab")))
    (is (= 7 (lps/longest-palindromic-subsequence "bbabcbcab")))
    (is (= 1 (lps/longest-palindromic-subsequence "a")))
    (is (= 0 (lps/longest-palindromic-subsequence "")))
    (is (= 7 (lps/longest-palindromic-subsequence "racecar")))))
