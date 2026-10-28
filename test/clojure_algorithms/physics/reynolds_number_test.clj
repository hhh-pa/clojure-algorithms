(ns clojure-algorithms.physics.reynolds-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.reynolds-number :refer [reynolds-number]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-reynolds-number
  (testing "known values"
    (is (approx= (reynolds-number 900 2.5 0.05 0.4) 281.25 1e-9))
    (is (approx= (reynolds-number 450 3.86 0.078 0.23) 589.0695652173912 1e-9))
    (is (approx= (reynolds-number 234 -4.5 0.3 0.44) 717.9545454545454 1e-9)))
  (testing "non-positive inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (reynolds-number -90 2 0.045 1)))
    (is (thrown? clojure.lang.ExceptionInfo (reynolds-number 0 2 -0.4 -2)))))
