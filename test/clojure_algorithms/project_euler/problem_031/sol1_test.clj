(ns clojure-algorithms.project-euler.problem-031.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-031.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 11 (solution 10)))
  (is (= 451 (solution 50))))
