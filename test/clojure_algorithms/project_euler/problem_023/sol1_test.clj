(ns clojure-algorithms.project-euler.problem-023.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-023.sol1 :refer [solution]]))

(deftest solution-test
  ;; The full-scale default run is slow, so only tiny limits are tested.
  ;; Expected values verified against the Python implementation.
  (is (= 276 (solution 24)))
  (is (= 891 (solution 50))))
