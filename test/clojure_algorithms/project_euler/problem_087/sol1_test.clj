(ns clojure-algorithms.project-euler.problem-087.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-087.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 4 (solution 50))))
