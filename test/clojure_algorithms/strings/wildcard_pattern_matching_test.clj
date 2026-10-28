(ns clojure-algorithms.strings.wildcard-pattern-matching-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.wildcard-pattern-matching :refer [match-pattern]]))

(deftest test-match-pattern
  (testing "doctest examples"
    (is (true? (match-pattern "aab" "c*a*b")))
    (is (false? (match-pattern "dabc" "*abc")))
    (is (false? (match-pattern "aaa" "aa")))
    (is (true? (match-pattern "aaa" "a.a")))
    (is (false? (match-pattern "aaab" "aa*")))
    (is (true? (match-pattern "aaab" ".*")))
    (is (false? (match-pattern "a" "bbbb")))
    (is (false? (match-pattern "" "bbbb")))
    (is (false? (match-pattern "a" "")))
    (is (true? (match-pattern "" "")))))
