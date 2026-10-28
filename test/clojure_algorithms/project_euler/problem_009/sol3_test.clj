(ns clojure-algorithms.project-euler.problem-009.sol3-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-009.sol3 :refer [solution]]))

(deftest solution-test
  (is (= 31875000 (solution))))
