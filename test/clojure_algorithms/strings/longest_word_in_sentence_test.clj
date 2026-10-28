(ns clojure-algorithms.strings.longest-word-in-sentence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.longest-word-in-sentence :refer [longest-word]]))

(deftest test-longest-word
  (testing "doctest examples"
    (is (= "jumped" (longest-word "The quick brown fox jumped over the lazy dog")))
    (is (= "amazing" (longest-word "Python is amazing")))
    (is (= "" (longest-word "")))
    (is (= "a" (longest-word "a")))
    (is (= "thousand"
           (longest-word "A journey of a thousand miles begins with a single step")))
    (is (= "question" (longest-word "To be or not to be that is the question")))
    (is (= "beholder" (longest-word "Beauty is in the eye of the beholder")))
    (is (= "thousand" (longest-word "A picture is worth a thousand words")))
    (is (= "glitters" (longest-word "All that glitters is not gold")))))
