(ns clojure-algorithms.project-euler.problem-120.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-120.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 300 (solution 10)))
  (is (= 330750 (solution 100))))
