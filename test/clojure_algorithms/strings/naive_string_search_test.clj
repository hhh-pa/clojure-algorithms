(ns clojure-algorithms.strings.naive-string-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.naive-string-search :refer [naive-pattern-search]]))

(deftest test-naive-pattern-search
  (testing "doctest examples"
    (is (= [4 10 18]
           (naive-pattern-search "ABAAABCDBBABCDDEBCABC" "ABC")))
    (is (= [] (naive-pattern-search "ABC" "ABAAABCDBBABCDDEBCABC")))
    (is (= [] (naive-pattern-search "" "ABC")))
    (is (= [0] (naive-pattern-search "TEST" "TEST")))
    (is (= [7] (naive-pattern-search "ABCDEGFTEST" "TEST"))))
  (testing "main example"
    (is (= [3] (naive-pattern-search "ABCDEFG" "DE")))))
