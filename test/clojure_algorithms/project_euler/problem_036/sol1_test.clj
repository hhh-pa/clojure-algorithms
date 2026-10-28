(ns clojure-algorithms.project-euler.problem-036.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-036.sol1 :refer [palindrome? solution]]))

(deftest palindrome?-test
  (is (true? (palindrome? 909)))
  (is (false? (palindrome? 908)))
  (is (true? (palindrome? "10101")))
  (is (false? (palindrome? "10111"))))

(deftest solution-test
  (is (= 25 (solution 10)))
  (is (= 157 (solution 100)))
  (is (= 1772 (solution 1000))))
