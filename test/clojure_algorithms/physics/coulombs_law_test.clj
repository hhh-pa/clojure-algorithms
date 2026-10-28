(ns clojure-algorithms.physics.coulombs-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.coulombs-law :refer [coulombs-law]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-coulombs-law
  (testing "known values"
    (is (approx= (coulombs-law 15.5 20 15) 12382849136.06 1.0))
    (is (approx= (coulombs-law 1 15 5) 5392531075.38 1.0))
    (is (approx= (coulombs-law 20 -50 15) -39944674632.44 1.0))
    (is (approx= (coulombs-law -5 -8 10) 3595020716.92 1.0))
    (is (approx= (coulombs-law 50 100 50) 17975103584.6 1.0)))
  (testing "non-positive radius throws"
    (is (thrown? clojure.lang.ExceptionInfo (coulombs-law 1 1 0)))
    (is (thrown? clojure.lang.ExceptionInfo (coulombs-law 1 1 -5)))))
