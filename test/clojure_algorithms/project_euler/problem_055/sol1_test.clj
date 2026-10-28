(ns clojure-algorithms.project-euler.problem-055.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-055.sol1 :refer [palindrome? sum-reverse solution]]))

(deftest palindrome?-test
  (is (false? (palindrome? 12567321)))
  (is (true? (palindrome? 1221)))
  (is (true? (palindrome? 9876789))))

(deftest sum-reverse-test
  (is (= 444 (sum-reverse 123)))
  (is (= 12221 (sum-reverse 3478)))
  (is (= 33 (sum-reverse 12))))

(deftest solution-test
  (is (= 13 (solution 1000)))
  (is (= 76 (solution 5000))))
