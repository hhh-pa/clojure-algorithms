(ns clojure-algorithms.project-euler.problem-092.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-092.sol1 :refer [digit-square-sum
                                                                       solution]]))

(deftest digit-square-sum-test
  (is (= 32 (digit-square-sum 44)))
  (is (= 89 (digit-square-sum 85))))

(deftest solution-test
  (is (= 0 (solution 2)))
  (is (= 80 (solution 100))))
