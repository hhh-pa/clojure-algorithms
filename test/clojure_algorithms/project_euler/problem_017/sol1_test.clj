(ns clojure-algorithms.project-euler.problem-017.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-017.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 21124 (solution 1000)))
  (is (= 19 (solution 5))))
