(ns clojure-algorithms.project-euler.problem-026.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-026.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 7 (solution 1 10)))
  (is (= 97 (solution 10 100))))
