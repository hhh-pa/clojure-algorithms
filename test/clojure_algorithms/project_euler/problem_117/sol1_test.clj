(ns clojure-algorithms.project-euler.problem-117.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-117.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 15 (solution 5))))
