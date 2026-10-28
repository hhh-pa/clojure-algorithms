(ns clojure-algorithms.physics.horizontal-projectile-motion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.horizontal-projectile-motion
             :refer [horizontal-distance max-height total-time]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-horizontal-distance
  (testing "known values"
    (is (approx= (horizontal-distance 30 45) 91.77 1e-9))
    (is (approx= (horizontal-distance 100 78) 414.76 1e-9))
    (is (approx= (horizontal-distance 25 20) 40.97 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (horizontal-distance -1 20)))
    (is (thrown? clojure.lang.ExceptionInfo (horizontal-distance 30 -20)))
    (is (thrown? clojure.lang.ExceptionInfo (horizontal-distance 30 "b")))))

(deftest test-max-height
  (testing "known values"
    (is (approx= (max-height 30 45) 22.94 1e-9))
    (is (approx= (max-height 100 78) 487.82 1e-9))
    (is (approx= (max-height 25 20) 3.73 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (max-height "a" 20)))))

(deftest test-total-time
  (testing "known values"
    (is (approx= (total-time 30 45) 4.33 1e-9))
    (is (approx= (total-time 100 78) 19.95 1e-9))
    (is (approx= (total-time 25 20) 1.74 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (total-time -10 40)))
    (is (thrown? clojure.lang.ExceptionInfo (total-time 30 "b")))))
