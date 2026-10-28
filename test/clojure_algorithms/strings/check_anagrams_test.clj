(ns clojure-algorithms.strings.check-anagrams-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.check-anagrams :refer [check-anagrams]]))

(deftest test-check-anagrams
  (testing "doctest examples"
    (is (true? (check-anagrams "Silent" "Listen")))
    (is (true? (check-anagrams "This is a string" "Is this a string")))
    (is (true? (check-anagrams "This is    a      string"
                               "Is     this a string")))
    (is (false? (check-anagrams "There" "Their"))))
  (testing "edges"
    (is (true? (check-anagrams "" "")))
    (is (false? (check-anagrams "abc" "abcd")))))
