(ns clojure-algorithms.project-euler.problem-009.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-009.sol2 :refer [solution]]))

(deftest solution-test
  (is (= 1620 (solution 36)))
  (is (= 66780 (solution 126))))
