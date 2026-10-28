(ns clojure-algorithms.backtracking.word-break-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.word-break :refer [backtrack word-break]]))

(deftest backtrack-test
  (is (true? (backtrack "leetcode" #{"leet" "code"} 0)))
  (is (true? (backtrack "applepenapple" #{"apple" "pen"} 0)))
  (is (false? (backtrack "catsandog" #{"cats" "dog" "sand" "and" "cat"} 0))))

(deftest word-break-test
  (testing "doctest examples"
    (is (true? (word-break "leetcode" #{"leet" "code"})))
    (is (true? (word-break "applepenapple" #{"apple" "pen"})))
    (is (false? (word-break "catsandog" #{"cats" "dog" "sand" "and" "cat"})))
    (is (false? (word-break "applepenapple" #{}))))
  (testing "edges"
    (is (true? (word-break "" #{"a"})))
    (is (true? (word-break "a" #{"a"})))))
