(ns clojure-algorithms.project-euler.problem-800.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-800.sol1 :refer [calculate-prime-numbers
                                                                       solution]]))

(deftest calculate-prime-numbers-test
  (is (= [2 3 5 7] (calculate-prime-numbers 10))))

(deftest solution-test
  (is (= 2 (solution 800 1)))
  (is (= 10790 (solution 800 800))))
