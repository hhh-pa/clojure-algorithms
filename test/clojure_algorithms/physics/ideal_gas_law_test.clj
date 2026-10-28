(ns clojure-algorithms.physics.ideal-gas-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.ideal-gas-law
             :refer [moles-of-gas-system pressure-of-gas-system
                     temperature-of-gas-system volume-of-gas-system]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-ideal-gas-law
  (testing "known values"
    (is (approx= (pressure-of-gas-system 2 100 5) 332.57848 1e-6))
    (is (approx= (pressure-of-gas-system 0.5 273 0.004) 283731.01575 1e-3))
    (is (approx= (volume-of-gas-system 2 100 5) 332.57848 1e-6))
    (is (approx= (volume-of-gas-system 0.5 273 0.004) 283731.01575 1e-3))
    (is (approx= (temperature-of-gas-system 2 100 5) 30.068090996146232 1e-9))
    (is (approx= (temperature-of-gas-system 11 5009 1000) 54767.66101807144 1e-6))
    (is (approx= (moles-of-gas-system 100 5 10) 0.06013618199229246 1e-12))
    (is (approx= (moles-of-gas-system 110 5009 1000) 5476.766101807144 1e-6)))
  (testing "negative inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (pressure-of-gas-system 3 -0.46 23.5)))
    (is (thrown? clojure.lang.ExceptionInfo (volume-of-gas-system 3 -0.46 23.5)))
    (is (thrown? clojure.lang.ExceptionInfo (temperature-of-gas-system 3 -0.46 23.5)))
    (is (thrown? clojure.lang.ExceptionInfo (moles-of-gas-system 3 -0.46 23.5)))))
