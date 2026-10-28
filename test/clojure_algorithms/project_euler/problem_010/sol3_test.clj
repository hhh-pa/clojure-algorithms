(ns clojure-algorithms.project-euler.problem-010.sol3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-010.sol3 :refer [solution]]))

(deftest solution-test
  (is (= 76127 (solution 1000)))
  (is (= 10 (solution 7)))
  (is (thrown? NegativeArraySizeException (solution -7))))
