(ns clojure-algorithms.project-euler.problem-137.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-137.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 104 (solution 3)))
  (is (= 74049690 (solution 10))))
