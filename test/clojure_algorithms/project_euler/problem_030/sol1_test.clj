(ns clojure-algorithms.project-euler.problem-030.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-030.sol1 :refer [digits-fifth-powers-sum]]))

(deftest digits-fifth-powers-sum-test
  (is (= 1300 (digits-fifth-powers-sum 1234))))
