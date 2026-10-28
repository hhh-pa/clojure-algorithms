(ns clojure-algorithms.project-euler.problem-001.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-001.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 0 (solution 3)))
  (is (= 3 (solution 4)))
  (is (= 23 (solution 10)))
  (is (= 83700 (solution 600)))
  (is (= 0 (solution -7))))
