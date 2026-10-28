(ns clojure-algorithms.conversions.astronomical-length-scale-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.astronomical-length-scale-conversion
             :refer [length-conversion]]))

(deftest test-length-conversion
  (is (== 0.001 (length-conversion 1 "meter" "kilometer")))
  (is (== 1e-06 (length-conversion 1 "meter" "megametre")))
  (is (== 1000000000 (length-conversion 1 "gigametre" "meter")))
  (is (== 0.001 (length-conversion 1 "gigametre" "terametre")))
  (is (== 1000 (length-conversion 1 "petametre" "terametre")))
  (is (== 0.001 (length-conversion 1 "petametre" "exametre")))
  (is (== 1e-09 (length-conversion 1 "terametre" "zettametre")))
  (is (== 1000 (length-conversion 1 "yottametre" "zettametre")))
  (is (== 0.002 (length-conversion 2 "meters" "kilometers")))
  (is (thrown? clojure.lang.ExceptionInfo
               (length-conversion 4 "wrongUnit" "inch"))))
