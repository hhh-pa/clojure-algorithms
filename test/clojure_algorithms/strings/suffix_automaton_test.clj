(ns clojure-algorithms.strings.suffix-automaton-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.suffix-automaton
             :refer [build-suffix-automaton sam-contains?
                     count-distinct-substrings count-occurrences]]))

(deftest test-sam
  (testing "doctest examples"
    (let [sam (build-suffix-automaton "abacaba")]
      (is (true? (sam-contains? sam "abac")))
      (is (true? (sam-contains? sam "caba")))
      (is (false? (sam-contains? sam "xyz")))
      (is (= 21 (count-distinct-substrings sam)))
      (is (= 2 (count-occurrences sam "abacaba" "aba")))
      (is (= 4 (count-occurrences sam "abacaba" "a"))))
    (is (thrown? IllegalArgumentException (build-suffix-automaton ""))))
  (testing "contains"
    (let [sam (build-suffix-automaton "banana")]
      (is (true? (sam-contains? sam "nan")))
      (is (false? (sam-contains? sam "apple")))))
  (testing "distinct substrings"
    (is (= 6 (count-distinct-substrings (build-suffix-automaton "abc"))))
    (is (= 4 (count-distinct-substrings (build-suffix-automaton "aaaa")))))
  (testing "occurrences"
    (let [sam (build-suffix-automaton "banana")]
      (is (= 2 (count-occurrences sam "banana" "an")))
      (is (= 2 (count-occurrences sam "banana" "na")))
      (is (= 1 (count-occurrences sam "banana" "banana")))
      (is (= 0 (count-occurrences sam "banana" "xyz"))))))
