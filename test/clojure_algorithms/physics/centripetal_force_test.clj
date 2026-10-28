(ns clojure-algorithms.physics.centripetal-force-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.centripetal-force :refer [centripetal]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-centripetal
  (testing "known values"
    (is (approx= (centripetal 15.5 -30 10) 1395.0 1e-6))
    (is (approx= (centripetal 10 15 5) 450.0 1e-6))
    (is (approx= (centripetal 20 -50 15) 3333.33 1e-2))
    (is (approx= (centripetal 12.25 40 25) 784.0 1e-6))
    (is (approx= (centripetal 50 100 50) 10000.0 1e-6)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (centripetal -1 10 5)))
    (is (thrown? clojure.lang.ExceptionInfo (centripetal 10 10 0)))
    (is (thrown? clojure.lang.ExceptionInfo (centripetal 10 10 -5)))))
