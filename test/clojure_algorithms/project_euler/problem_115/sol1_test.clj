(ns clojure-algorithms.project-euler.problem-115.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-115.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 30 (solution 3)))
  (is (= 57 (solution 10))))
