(ns clojure-algorithms.physics.escape-velocity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.escape-velocity :refer [escape-velocity]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-escape-velocity
  (testing "known values"
    (is (approx= (escape-velocity 5.972e24 6.371e6) 11185.978 1e-6))
    (is (approx= (escape-velocity 7.348e22 1.737e6) 2376.307 1e-6))
    (is (approx= (escape-velocity 1.898e27 6.9911e7) 60199.545 1e-3))
    (is (approx= (escape-velocity 0 1.0) 0.0 1e-9)))
  (testing "zero radius throws"
    (is (thrown? ArithmeticException (escape-velocity 1.0 0)))))
