(ns clojure-algorithms.project-euler.problem-345.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-345.sol1 :refer [matrix-1
                                                                       solution
                                                                       solve]]))

(deftest solve-test
  (is (= 5 (solve [[1 2] [3 4]] 0 #{0 1} (atom {})))))

(deftest solution-test
  (is (= 5 (solution ["1 2" "3 4"])))
  (is (= 3315 (solution matrix-1))))
