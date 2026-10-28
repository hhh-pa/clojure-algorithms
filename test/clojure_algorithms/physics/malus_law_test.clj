(ns clojure-algorithms.physics.malus-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.malus-law :refer [malus-law]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-malus-law
  (testing "known values"
    (is (approx= (malus-law 10 45) 5.0 1e-9))
    (is (approx= (malus-law 100 60) 25.0 1e-9))
    (is (approx= (malus-law 50 150) 37.5 1e-9))
    (is (approx= (malus-law 75 270) 0.0 1e-9))
    (is (approx= (malus-law 100 180) 100.0 1e-9))
    (is (approx= (malus-law 100 360) 100.0 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (malus-law 10 -900)))
    (is (thrown? clojure.lang.ExceptionInfo (malus-law 10 900)))
    (is (thrown? clojure.lang.ExceptionInfo (malus-law -100 900)))))
