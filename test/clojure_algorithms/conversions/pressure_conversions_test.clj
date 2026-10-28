(ns clojure-algorithms.conversions.pressure-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.pressure-conversions
             :refer [pressure-conversion]]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-pressure-conversion
  (is (approx 405300 (pressure-conversion 4 "atm" "pascal")))
  (is (approx 0.00014401981999999998 (pressure-conversion 1 "pascal" "psi")))
  (is (approx 0.986923 (pressure-conversion 1 "bar" "atm")))
  (is (approx 0.029999991892499998 (pressure-conversion 3 "kilopascal" "bar")))
  (is (approx 290.074434314 (pressure-conversion 2 "megapascal" "psi")))
  (is (approx 206.85984 (pressure-conversion 4 "psi" "torr")))
  (is (approx 0.0334211 (pressure-conversion 1 "inHg" "atm")))
  (is (approx 0.019336718261000002 (pressure-conversion 1 "torr" "psi")))
  (is (thrown? clojure.lang.ExceptionInfo
               (pressure-conversion 4 "wrongUnit" "atm"))))
