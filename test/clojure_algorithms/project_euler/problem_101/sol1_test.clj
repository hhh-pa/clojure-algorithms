(ns clojure-algorithms.project-euler.problem-101.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-101.sol1 :refer [interpolate
                                                                       question-function
                                                                       solution
                                                                       solve]]))

(deftest solve-test
  (is (= [[1.0] [2.0]] (solve [[1 0] [0 1]] [[1] [2]])))
  (is (= [[2.0] [3.0] [-1.0]]
         (solve [[2 1 -1] [-3 -1 2] [-2 1 2]] [[8] [-11] [-3]]))))

(deftest interpolate-test
  (is (= 1 ((interpolate [1]) 3)))
  (is (= 15 ((interpolate [1 8]) 3)))
  (is (= 58 ((interpolate [1 8 27]) 4)))
  (is (= 216 ((interpolate [1 8 27 64]) 6))))

(deftest question-function-test
  (is (= 1 (question-function 0)))
  (is (= 1 (question-function 1)))
  (is (= 8138021 (question-function 5)))
  (is (= 9090909091 (question-function 10))))

(deftest solution-test
  (is (= 74 (solution (fn [n] (* n n n)) 3))))
