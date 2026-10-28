(ns clojure-algorithms.project-euler.problem-138.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-138.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 322 (solution 2)))
  (is (= 1866293 (solution 5))))
