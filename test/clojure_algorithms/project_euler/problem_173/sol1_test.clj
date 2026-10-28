(ns clojure-algorithms.project-euler.problem-173.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-173.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 41 (solution 100))))
