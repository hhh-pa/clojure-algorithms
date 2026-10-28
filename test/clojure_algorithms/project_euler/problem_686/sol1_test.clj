(ns clojure-algorithms.project-euler.problem-686.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-686.sol1 :refer [log-difference
                                                                       solution]]))

(deftest log-difference-test
  (is (= 0.092699609758302 (log-difference 90)))
  (is (= 0.090368356648852 (log-difference 379))))

(deftest solution-test
  (is (= 2515 (solution 10)))
  (is (= 284168 (solution 1000))))
