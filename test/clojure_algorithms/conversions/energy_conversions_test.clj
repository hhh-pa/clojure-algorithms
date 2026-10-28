(ns clojure-algorithms.conversions.energy-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.energy-conversions
             :refer [energy-conversion]]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-energy-conversion
  (is (= 1.0 (energy-conversion "joule" "joule" 1)))
  (is (= 0.001 (energy-conversion "joule" "kilojoule" 1)))
  (is (= 1e-06 (energy-conversion "joule" "megajoule" 1)))
  (is (= 1e-09 (energy-conversion "joule" "gigajoule" 1)))
  (is (= 1.0 (energy-conversion "joule" "wattsecond" 1)))
  (is (approx 0.0002777777777777778
              (energy-conversion "joule" "watthour" 1)))
  (is (approx 2.7777777777777776e-07
              (energy-conversion "joule" "kilowatthour" 1)))
  (is (= 1.0 (energy-conversion "joule" "newtonmeter" 1)))
  (is (approx 0.00023884589662749592
              (energy-conversion "joule" "calorie_nutr" 1)))
  (is (approx 6.241509074460763e+18
              (energy-conversion "joule" "electronvolt" 1)))
  (is (approx 0.0009478171226670134
              (energy-conversion "joule" "britishthermalunit_it" 1)))
  (is (approx 0.7375621211696556
              (energy-conversion "joule" "footpound" 1)))
  (is (= 0.001 (energy-conversion "joule" "megajoule" 1000)))
  (is (= 1.0 (energy-conversion "calorie_nutr" "kilocalorie_nutr" 1000)))
  (is (= 36000000.0 (energy-conversion "kilowatthour" "joule" 10)))
  (is (approx 778.1692306784539
              (energy-conversion "britishthermalunit_it" "footpound" 1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (energy-conversion "watthour" "joule" "a")))
  (is (thrown? clojure.lang.ExceptionInfo
               (energy-conversion "wrongunit" "joule" 1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (energy-conversion "joule" "wrongunit" 1))))
