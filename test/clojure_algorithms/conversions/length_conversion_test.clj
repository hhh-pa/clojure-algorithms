(ns clojure-algorithms.conversions.length-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.length-conversion
             :refer [length-conversion]]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-length-conversion
  (is (approx 13.12336 (length-conversion 4 "METER" "FEET")))
  (is (approx 13.12336 (length-conversion 4 "M" "FT")))
  (is (approx 0.001 (length-conversion 1 "meter" "kilometer")))
  (is (approx 39370.1 (length-conversion 1 "kilometer" "inch")))
  (is (approx 1.8641130000000001 (length-conversion 3 "kilometer" "mile")))
  (is (approx 0.6096 (length-conversion 2 "feet" "meter")))
  (is (approx 1.333329312 (length-conversion 4 "feet" "yard")))
  (is (approx 0.0254 (length-conversion 1 "inch" "meter")))
  (is (approx 3.15656468e-05 (length-conversion 2 "inch" "mile")))
  (is (approx 20.0 (length-conversion 2 "centimeter" "millimeter")))
  (is (approx 0.0218722 (length-conversion 2 "centimeter" "yard")))
  (is (approx 3.6576 (length-conversion 4 "yard" "meter")))
  (is (approx 0.0036576 (length-conversion 4 "yard" "kilometer")))
  (is (approx 0.9144000000000001 (length-conversion 3 "foot" "meter")))
  (is (approx 36.00001944 (length-conversion 3 "foot" "inch")))
  (is (approx 6.43736 (length-conversion 4 "mile" "kilometer")))
  (is (approx 126719.753468 (length-conversion 2 "miles" "InChEs")))
  (is (approx 0.3 (length-conversion 3 "millimeter" "centimeter")))
  (is (approx 0.1181103 (length-conversion 3 "mm" "in")))
  (is (thrown? clojure.lang.ExceptionInfo
               (length-conversion 4 "wrongUnit" "inch"))))
