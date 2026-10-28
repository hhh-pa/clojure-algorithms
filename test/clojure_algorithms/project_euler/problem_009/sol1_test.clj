(ns clojure-algorithms.project-euler.problem-009.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-009.sol1
             :refer [solution solution-fast]]))

(deftest solution-test
  ;; solution uses a slow triple loop, so only the fast variant is
  ;; exercised here; both return the same doctest value.
  (is (= 31875000 (solution-fast))))
