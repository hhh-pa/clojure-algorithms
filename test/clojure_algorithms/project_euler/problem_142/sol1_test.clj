(ns clojure-algorithms.project-euler.problem-142.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-142.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 1 (solution 1)))
  (is (= 9 (solution 2))))
