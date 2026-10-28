(ns clojure-algorithms.project-euler.problem-065.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-065.sol1 :refer [solution sum-digits]]))

(deftest sum-digits-test
  (is (= 1 (sum-digits 1)))
  (is (= 15 (sum-digits 12345)))
  (is (= 28 (sum-digits 999001))))

(deftest solution-test
  (is (= 13 (solution 9)))
  (is (= 17 (solution 10)))
  (is (= 91 (solution 50))))
