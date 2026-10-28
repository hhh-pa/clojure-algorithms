(ns clojure-algorithms.project-euler.problem-187.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-187.sol1 :refer [calculate-prime-numbers
                                                                       slow-calculate-prime-numbers
                                                                       slow-solution
                                                                       solution
                                                                       while-solution]]))

(deftest slow-calculate-prime-numbers-test
  (is (= [2 3 5 7] (slow-calculate-prime-numbers 10)))
  (is (= [] (slow-calculate-prime-numbers 2))))

(deftest calculate-prime-numbers-test
  (is (= [2 3 5 7] (calculate-prime-numbers 10)))
  (is (= [] (calculate-prime-numbers 2))))

(deftest slow-solution-test
  (is (= 10 (slow-solution 30))))

(deftest while-solution-test
  (is (= 10 (while-solution 30))))

(deftest solution-test
  (is (= 10 (solution 30))))
