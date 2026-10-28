(ns clojure-algorithms.physics.faraday-lenz-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.faraday-lenz-law
             :refer [induced-electromotive-force]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-induced-electromotive-force
  (testing "known values"
    (is (approx= (induced-electromotive-force 50.0 20 3.0) -10.0 1e-9))
    (is (approx= (induced-electromotive-force 40 30 10.0) -1.0 1e-9))
    (is (approx= (induced-electromotive-force 30.0 50.0 10) 2.0 1e-9))
    (is (approx= (induced-electromotive-force 100 100.0 20.0) -0.0 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (induced-electromotive-force 10.0 2.0 -2.0)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (induced-electromotive-force 11.0 "a" 5.0)))))
