(ns clojure-algorithms.project-euler.problem-164.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-164.sol1 :refer [solution
                                                                       solve]]))

(deftest solve-test
  (is (= 9 (solve 1 0 0 9 true (atom {}))))
  (is (= 10 (solve 1 0 0 9 false (atom {})))))

(deftest solution-test
  (is (= 45 (solution 2)))
  (is (= 21838806 (solution 10))))
