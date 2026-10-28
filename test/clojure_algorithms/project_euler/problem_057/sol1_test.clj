(ns clojure-algorithms.project-euler.problem-057.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-057.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 2 (solution 14)))
  (is (= 15 (solution 100))))
