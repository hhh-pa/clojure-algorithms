(ns clojure-algorithms.project-euler.problem-091.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-091.sol1 :refer [is-right solution]]))

(deftest is-right-test
  (is (true? (is-right 0 1 2 0)))
  (is (false? (is-right 1 0 2 2))))

(deftest solution-test
  (is (= 14 (solution 2)))
  (is (= 448 (solution 10))))
