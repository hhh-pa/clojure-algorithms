(ns clojure-algorithms.project-euler.problem-021.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-021.sol1
             :refer [solution sum-of-divisors]]))

(deftest sum-of-divisors-test
  (is (= 284 (sum-of-divisors 220)))
  (is (= 220 (sum-of-divisors 284))))

(deftest solution-test
  (is (= 504 (solution 1000)))
  (is (= 0 (solution 100)))
  (is (= 0 (solution 50))))
