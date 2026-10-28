(ns clojure-algorithms.project-euler.problem-013.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-013.sol1 :refer [solution]]))

(deftest solution-test
  (is (= "5537376230" (solution "5537376230\n0")))
  (is (= "1000000000" (solution "9999999999\n1"))))
