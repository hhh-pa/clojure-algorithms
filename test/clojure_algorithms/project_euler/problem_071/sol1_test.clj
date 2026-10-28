(ns clojure-algorithms.project-euler.problem-071.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-071.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 2 (solution 3 7 8)))
  (is (= 47 (solution 6 7 60))))
