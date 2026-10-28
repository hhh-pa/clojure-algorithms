(ns clojure-algorithms.project-euler.problem-039.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-039.sol1 :refer [pythagorean-triple solution]]))

(deftest pythagorean-triple-test
  (is (= {12 1} (pythagorean-triple 15))))

(deftest solution-test
  (is (= 90 (solution 100)))
  (is (= 180 (solution 200))))
