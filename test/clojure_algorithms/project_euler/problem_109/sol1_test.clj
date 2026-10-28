(ns clojure-algorithms.project-euler.problem-109.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-109.sol1 :refer [solution]]))

(deftest solution-test
  (is (= 12577 (solution 50)))
  (is (= 42336 (solution 171))))
