(ns clojure-algorithms.project-euler.problem-029.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-029.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 15 (solution 5)))
  (is (= 1 (solution 2)))
  (is (= 0 (solution 1)))
  (is (= 324 (solution 20))))
