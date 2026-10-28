(ns clojure-algorithms.project-euler.problem-207.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-207.sol1 :refer [check-partition-perfect
                                                                       solution]]))

(deftest check-partition-perfect-test
  (is (true? (check-partition-perfect 2)))
  (is (false? (check-partition-perfect 6))))

(deftest solution-test
  (is (= 6 (solution 1.0)))
  (is (= 30 (solution (/ 1.0 2))))
  (is (= 210 (solution (/ 3.0 13)))))
