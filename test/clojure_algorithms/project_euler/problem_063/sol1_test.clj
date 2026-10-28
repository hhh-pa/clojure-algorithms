(ns clojure-algorithms.project-euler.problem-063.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-063.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 49 (solution 10 22)))
  (is (= 0 (solution 0 0)))
  (is (= 0 (solution 1 1)))
  (is (= 0 (solution -1 -1))))
