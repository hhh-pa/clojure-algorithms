(ns clojure-algorithms.backtracking.match-word-pattern-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.match-word-pattern :refer [match-word-pattern]]))

(deftest match-word-pattern-test
  (testing "doctest examples"
    (is (true? (match-word-pattern "aba" "GraphTreesGraph")))
    (is (true? (match-word-pattern "xyx" "PythonRubyPython")))
    (is (false? (match-word-pattern "GG" "PythonJavaPython"))))
  (testing "edges"
    (is (true? (match-word-pattern "" "")))
    (is (false? (match-word-pattern "a" "")))
    (is (false? (match-word-pattern "" "abc")))
    (is (true? (match-word-pattern "aaaa" "TTTT")))
    (is (false? (match-word-pattern "ab" "aa")))))
