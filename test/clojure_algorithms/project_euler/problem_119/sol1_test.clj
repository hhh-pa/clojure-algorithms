(ns clojure-algorithms.project-euler.problem-119.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-119.sol1 :refer [digit-sum
                                                                       solution]]))

(deftest digit-sum-test
  (is (= 6 (digit-sum 123)))
  (is (= 15 (digit-sum 456)))
  (is (= 25 (digit-sum 78910))))

(deftest solution-test
  (is (= 512 (solution 2)))
  (is (= 5832 (solution 5)))
  (is (= 614656 (solution 10))))
