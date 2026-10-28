(ns clojure-algorithms.project-euler.problem-089.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-089.sol1 :refer [generate-roman-numerals
                                                                       parse-roman-numerals
                                                                       solution]]))

(deftest parse-roman-numerals-test
  (is (= 89 (parse-roman-numerals "LXXXIX")))
  (is (= 4 (parse-roman-numerals "IIII"))))

(deftest generate-roman-numerals-test
  (is (= "LXXXIX" (generate-roman-numerals 89)))
  (is (= "IV" (generate-roman-numerals 4))))

(deftest solution-test
  (is (= 16 (solution "IIII\nIV\nIIIIIIIIII\nX\nVIIIII"))))
