(ns clojure-algorithms.project-euler.problem-080.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-080.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 40886 (solution))))
