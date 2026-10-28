(ns clojure-algorithms.project-euler.problem-125.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-125.sol1 :refer [is-palindrome
                                                                       solution]]))

(deftest is-palindrome-test
  (is (true? (is-palindrome 12521)))
  (is (false? (is-palindrome 12522)))
  (is (false? (is-palindrome 12210))))

(deftest solution-test
  (is (= 4164 (solution 1000))))
