(ns clojure-algorithms.strings.top-k-frequent-words-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.top-k-frequent-words
             :refer [top-k-frequent-words]]))

(deftest test-top-k-frequent-words
  (testing "doctest examples"
    (is (= ["c" "a" "b"]
           (top-k-frequent-words ["a" "b" "c" "a" "c" "c"] 3)))
    (is (= ["c" "a"]
           (top-k-frequent-words ["a" "b" "c" "a" "c" "c"] 2)))
    (is (= ["c"] (top-k-frequent-words ["a" "b" "c" "a" "c" "c"] 1)))
    (is (= [] (top-k-frequent-words ["a" "b" "c" "a" "c" "c"] 0)))
    (is (= [] (top-k-frequent-words [] 1)))
    (is (= ["a"] (top-k-frequent-words ["a" "a"] 2)))))
