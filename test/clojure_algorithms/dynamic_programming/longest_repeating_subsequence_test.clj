(ns clojure-algorithms.dynamic-programming.longest-repeating-subsequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.longest-repeating-subsequence :as lrs]))

(deftest lrs-test
  (testing "repeating subsequence lengths"
    (is (= 2 (lrs/longest-repeating-subsequence "aabb")))
    (is (= 1 (lrs/longest-repeating-subsequence "aab")))
    (is (= 2 (lrs/longest-repeating-subsequence "axxxy")))
    (is (= 3 (lrs/longest-repeating-subsequence "abcabc")))
    (is (= 0 (lrs/longest-repeating-subsequence "")))
    (is (= 0 (lrs/longest-repeating-subsequence "a")))
    (is (= 0 (lrs/longest-repeating-subsequence "abcdef")))
    (is (= 2 (lrs/longest-repeating-subsequence "aaa")))
    (is (= 3 (lrs/longest-repeating-subsequence "aaaa"))))
  (testing "non-string input"
    (is (thrown? IllegalArgumentException
                 (lrs/longest-repeating-subsequence 12345)))))
