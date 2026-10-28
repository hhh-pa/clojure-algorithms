(ns clojure-algorithms.strings.count-vowels-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.count-vowels :refer [count-vowels]]))

(deftest test-count-vowels
  (testing "doctest examples"
    (is (= 3 (count-vowels "hello world")))
    (is (= 3 (count-vowels "HELLO WORLD")))
    (is (= 3 (count-vowels "123 hello world")))
    (is (= 0 (count-vowels "")))
    (is (= 5 (count-vowels "a quick brown fox")))
    (is (= 5 (count-vowels "the quick BROWN fox")))
    (is (= 1 (count-vowels "PYTHON"))))
  (testing "non-string input throws"
    (is (thrown? IllegalArgumentException (count-vowels 123)))))
