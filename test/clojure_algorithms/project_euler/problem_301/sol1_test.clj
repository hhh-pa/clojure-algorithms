(ns clojure-algorithms.project-euler.problem-301.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-301.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 1 (solution 0)))
  (is (= 3 (solution 2)))
  (is (= 144 (solution 10))))
