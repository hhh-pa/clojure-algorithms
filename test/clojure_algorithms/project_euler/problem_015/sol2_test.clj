(ns clojure-algorithms.project-euler.problem-015.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-015.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 924 (solution 6)))
  (is (= 6 (solution 2)))
  (is (= 2 (solution 1))))
