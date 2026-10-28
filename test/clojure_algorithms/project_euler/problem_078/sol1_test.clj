(ns clojure-algorithms.project-euler.problem-078.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-078.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 1 (solution 1)))
  (is (= 14 (solution 9))))
