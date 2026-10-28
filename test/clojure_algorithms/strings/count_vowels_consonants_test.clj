(ns clojure-algorithms.strings.count-vowels-consonants-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.count-vowels-consonants
             :refer [count-vowels-and-consonants]]))

(deftest test-count-vowels-and-consonants
  (testing "doctest examples"
    (is (= [3 7] (count-vowels-and-consonants "Hello World")))
    (is (= [5 0] (count-vowels-and-consonants "AEIOU")))
    (is (= [0 4] (count-vowels-and-consonants "bcdf")))
    (is (= [0 0] (count-vowels-and-consonants "")))))
