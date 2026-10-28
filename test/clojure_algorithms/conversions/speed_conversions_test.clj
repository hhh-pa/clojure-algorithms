(ns clojure-algorithms.conversions.speed-conversions-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.speed-conversions :refer [convert-speed]]))

(deftest test-convert-speed
  (is (= 27.778 (convert-speed 100 "km/h" "m/s")))
  (is (= 62.137 (convert-speed 100 "km/h" "mph")))
  (is (= 53.996 (convert-speed 100 "km/h" "knot")))
  (is (= 360.0 (convert-speed 100 "m/s" "km/h")))
  (is (= 223.694 (convert-speed 100 "m/s" "mph")))
  (is (= 194.384 (convert-speed 100 "m/s" "knot")))
  (is (= 160.934 (convert-speed 100 "mph" "km/h")))
  (is (= 44.704 (convert-speed 100 "mph" "m/s")))
  (is (= 86.898 (convert-speed 100 "mph" "knot")))
  (is (= 185.2 (convert-speed 100 "knot" "km/h")))
  (is (= 51.444 (convert-speed 100 "knot" "m/s")))
  (is (= 115.078 (convert-speed 100 "knot" "mph")))
  (is (thrown? clojure.lang.ExceptionInfo
               (convert-speed 100 "km/h" "lightyear"))))
