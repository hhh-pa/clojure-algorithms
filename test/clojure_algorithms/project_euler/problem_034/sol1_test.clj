(ns clojure-algorithms.project-euler.problem-034.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-034.sol1 :refer [sum-of-digit-factorial]]))

(deftest sum-of-digit-factorial-test
  (is (= 121 (sum-of-digit-factorial 15)))
  (is (= 1 (sum-of-digit-factorial 0))))
