(ns clojure-algorithms.project-euler.problem-024.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-024.sol1 :refer [solution]]))

(deftest solution-test
  ;; The full-scale default run is slow, so only tiny inputs are tested.
  (is (= "012" (solution "012" 1)))
  (is (= "021" (solution "012" 2)))
  (is (= "102" (solution "012" 3)))
  (is (= "120" (solution "012" 4)))
  (is (= "201" (solution "012" 5)))
  (is (= "210" (solution "012" 6)))
  (is (= "0123" (solution "0123" 1)))
  (is (= "3210" (solution "0123" 24))))
