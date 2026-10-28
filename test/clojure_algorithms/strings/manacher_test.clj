(ns clojure-algorithms.strings.manacher-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.manacher :refer [palindromic-string]]))

(deftest test-palindromic-string
  (testing "doctest examples"
    (is (= "abbba" (palindromic-string "abbbaba")))
    (is (= "ababa" (palindromic-string "ababa"))))
  (testing "more cases (matching the Python original)"
    (is (= "b" (palindromic-string "abc")))
    (is (= "b" (palindromic-string "abb")))
    (is (= "racecar" (palindromic-string "racecar")))
    (is (= "" (palindromic-string "")))))
