(ns clojure-algorithms.project-euler.problem-144.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-144.sol1 :refer [next-point
                                                                       solution]]))

(defn- approx=
  "Returns true when every pair of numbers differs by less than tol."
  [expected actual tol]
  (every? true? (map (fn [e a] (< (Math/abs (- (double e) (double a))) tol))
                     expected
                     actual)))

(deftest next-point-test
  (is (approx= [-5.0 0.0 0.0] (next-point 5.0 0.0 0.0) 1e-9))
  (is (approx= [0.0 -10.0 2.0] (next-point 5.0 0.0 -2.0) 1e-9)))

(deftest solution-test
  (is (= 1 (solution 0.00001 -10)))
  (is (= 287 (solution 5 0))))
