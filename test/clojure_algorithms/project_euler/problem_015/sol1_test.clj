(ns clojure-algorithms.project-euler.problem-015.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-015.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 126410606437752 (solution 25)))
  (is (= 8233430727600 (solution 23)))
  (is (= 137846528820 (solution 20)))
  (is (= 155117520 (solution 15)))
  (is (= 2 (solution 1))))
