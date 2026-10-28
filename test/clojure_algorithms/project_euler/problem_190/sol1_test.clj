(ns clojure-algorithms.project-euler.problem-190.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-190.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 1 (solution 2)))
  (is (= 2 (solution 3)))
  (is (= 4 (solution 4)))
  (is (= 10 (solution 5))))
