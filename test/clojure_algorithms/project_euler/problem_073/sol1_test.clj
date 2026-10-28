(ns clojure-algorithms.project-euler.problem-073.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-073.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 0 (solution 4)))
  (is (= 1 (solution 5)))
  (is (= 3 (solution 8))))
