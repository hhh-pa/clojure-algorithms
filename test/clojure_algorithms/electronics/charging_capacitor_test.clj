(ns clojure-algorithms.electronics.charging-capacitor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.charging-capacitor :as cc]))

(defn- approx [expected actual]
  (< (Math/abs (- expected actual)) 1e-9))

(deftest charging-capacitor-test
  (testing "charging voltages"
    (is (approx 0.013 (cc/charging-capacitor 0.2 0.9 8.4 0.5)))
    (is (approx 1.446 (cc/charging-capacitor 2.2 3.5 2.4 9)))
    (is (approx 0.007 (cc/charging-capacitor 15 200 20 2)))
    (is (approx 19.975 (cc/charging-capacitor 20 2000 (* 30 1e-5) 4))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (cc/charging-capacitor 0 10.0 0.30 3)))
    (is (thrown? IllegalArgumentException (cc/charging-capacitor 20 -2000 30 4)))
    (is (thrown? IllegalArgumentException (cc/charging-capacitor 30 1500 0 4)))))
