(ns clojure-algorithms.project-euler.problem-205.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-205.sol1 :refer [solution
                                                                       total-frequency-distribution]]))

(deftest total-frequency-distribution-test
  (is (= [0 1 1 1 1 1 1] (total-frequency-distribution 6 1)))
  (is (= [0 0 1 2 3 4 3 2 1] (total-frequency-distribution 4 2))))

(deftest solution-test
  (is (= 0.5731441 (solution))))
