(ns clojure-algorithms.project-euler.problem-013.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-013.sol2 :refer [solution]]))

(deftest solution-test
  (is (= "5537376230" (solution "5537376230\n0")))
  (is (= "553" (solution "5537376230\n0" 3)))
  (is (= "553737" (solution "5537376230\n0" 6)))
  (is (= "1000000000" (solution "9999999999\n1"))))
