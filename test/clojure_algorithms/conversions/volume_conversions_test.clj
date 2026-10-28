(ns clojure-algorithms.conversions.volume-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.volume-conversions :refer [volume-conversion]]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-volume-conversion
  (is (approx 4000 (volume-conversion 4 "cubic meter" "litre")))
  (is (approx 0.264172 (volume-conversion 1 "litre" "gallon")))
  (is (approx 1 (volume-conversion 1 "kilolitre" "cubic meter")))
  (is (approx 0.017814279 (volume-conversion 3 "gallon" "cubic yard")))
  (is (approx 1529.1 (volume-conversion 2 "cubic yard" "litre")))
  (is (approx 473.396 (volume-conversion 4 "cubic foot" "cup")))
  (is (approx 0.000236588 (volume-conversion 1 "cup" "kilolitre")))
  (is (thrown? clojure.lang.ExceptionInfo
               (volume-conversion 4 "wrongUnit" "litre"))))
