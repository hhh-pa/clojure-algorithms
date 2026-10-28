(ns clojure-algorithms.project-euler.problem-121.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-121.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 10 (solution 4)))
  (is (= 225 (solution 10))))
