(ns clojure-algorithms.project-euler.problem-064.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-064.sol1 :refer [continuous-fraction-period
                                                                       solution]]))

(deftest continuous-fraction-period-test
  (is (= 1 (continuous-fraction-period 2)))
  (is (= 1 (continuous-fraction-period 5)))
  (is (= 4 (continuous-fraction-period 7)))
  (is (= 2 (continuous-fraction-period 11)))
  (is (= 5 (continuous-fraction-period 13))))

(deftest solution-test
  (is (= 1 (solution 2)))
  (is (= 2 (solution 5)))
  (is (= 2 (solution 7)))
  (is (= 3 (solution 11)))
  (is (= 4 (solution 13))))
