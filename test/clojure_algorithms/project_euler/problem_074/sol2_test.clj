(ns clojure-algorithms.project-euler.problem-074.sol2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-074.sol2 :refer [digit-factorial-sum
                                                                       solution]]))

(deftest digit-factorial-sum-test
  (is (= 1 (digit-factorial-sum 0)))
  (is (= 363600 (digit-factorial-sum 69)))
  (is (thrown? IllegalArgumentException (digit-factorial-sum 69.0)))
  (is (thrown? IllegalArgumentException (digit-factorial-sum -1))))

(deftest solution-test
  (is (= 26 (solution 10 1000)))
  (is (thrown? IllegalArgumentException (solution 10.0 1000)))
  (is (thrown? IllegalArgumentException (solution 10 1000.0)))
  (is (thrown? IllegalArgumentException (solution 0 1000)))
  (is (thrown? IllegalArgumentException (solution 10 0))))
