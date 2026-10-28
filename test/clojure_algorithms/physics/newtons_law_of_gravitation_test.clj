(ns clojure-algorithms.physics.newtons-law-of-gravitation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.newtons-law-of-gravitation
             :refer [gravitational-law]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-gravitational-law
  (testing "known values"
    (is (approx= (:force (gravitational-law 0 5 10 20)) 8.342875e-12 1e-21))
    (is (approx= (:mass-1 (gravitational-law 7367.382 0 74 3048))
                 1.385816317292268e+19 1e6))
    (let [d (:distance (gravitational-law 8.342875e-12 5 10 0))]
      (is (approx= d 20.0 1e-6)))
    (let [m (:mass-2 (gravitational-law 8.342875e-12 5 0 20))]
      (is (approx= m 10.0 1e-6))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (gravitational-law 36337.283 0 0 35584)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (gravitational-law 36337.283 -674 0 35584)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (gravitational-law -847938e12 674 0 9374)))))
