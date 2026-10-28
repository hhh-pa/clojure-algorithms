(ns clojure-algorithms.physics.doppler-frequency-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.doppler-frequency :refer [doppler-effect]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-doppler-effect
  (testing "known values"
    (is (approx= (doppler-effect 100 330 10 0) 103.03030303030303 1e-9))
    (is (approx= (doppler-effect 100 330 -10 0) 96.96969696969697 1e-9))
    (is (approx= (doppler-effect 100 330 0 10) 103.125 1e-9))
    (is (approx= (doppler-effect 100 330 0 -10) 97.05882352941177 1e-9))
    (is (approx= (doppler-effect 100 330 10 10) 106.25 1e-9))
    (is (approx= (doppler-effect 100 330 -10 -10) 94.11764705882354 1e-9)))
  (testing "degenerate inputs throw"
    (is (thrown? ArithmeticException (doppler-effect 100 330 10 330)))
    (is (thrown? clojure.lang.ExceptionInfo (doppler-effect 100 330 10 340)))
    (is (thrown? clojure.lang.ExceptionInfo (doppler-effect 100 330 -340 10)))))
