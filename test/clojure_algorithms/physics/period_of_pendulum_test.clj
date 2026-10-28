(ns clojure-algorithms.physics.period-of-pendulum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.period-of-pendulum :refer [period-of-pendulum]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-period-of-pendulum
  (testing "known values"
    (is (approx= (period-of-pendulum 1.23) 2.2252155506257845 1e-12))
    (is (approx= (period-of-pendulum 2.37) 3.0888278441908574 1e-12))
    (is (approx= (period-of-pendulum 5.63) 4.76073193364765 1e-12))
    (is (approx= (period-of-pendulum 0) 0.0 1e-9)))
  (testing "negative length throws"
    (is (thrown? clojure.lang.ExceptionInfo (period-of-pendulum -12)))))
