(ns clojure-algorithms.electronics.ic-555-timer-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.ic-555-timer :as t]))

(defn- approx [expected actual]
  (< (Math/abs (- expected actual)) 1e-9))

(deftest ic-555-timer-test
  (testing "frequency and duty cycle"
    (is (approx 1523.8095238095239 (t/astable-frequency 45 45 7)))
    (is (approx 1.7905459175553078 (t/astable-frequency 356 234 976)))
    (is (approx 66.66666666666666 (t/astable-duty-cycle 45 45)))
    (is (approx 71.60194174757282 (t/astable-duty-cycle 356 234))))
  (testing "invalid inputs"
    (is (thrown? IllegalArgumentException (t/astable-frequency 2 -1 2)))
    (is (thrown? IllegalArgumentException (t/astable-frequency 45 45 0)))
    (is (thrown? IllegalArgumentException (t/astable-duty-cycle 2 -1)))
    (is (thrown? IllegalArgumentException (t/astable-duty-cycle 0 0)))))
