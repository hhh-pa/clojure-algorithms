(ns clojure-algorithms.conversions.weight-conversion-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.conversions.weight-conversion :refer [weight-conversion]]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-6))

(deftest test-weight-conversion
  (is (= 4 (weight-conversion "kilogram" "kilogram" 4)))
  (is (= 1000 (weight-conversion "kilogram" "gram" 1)))
  (is (= 4000000 (weight-conversion "kilogram" "milligram" 4)))
  (is (= 0.004 (weight-conversion "kilogram" "metric-ton" 4)))
  (is (approx 0.0029526219 (weight-conversion "kilogram" "long-ton" 3)))
  (is (approx 0.0011023122 (weight-conversion "kilogram" "short-ton" 1)))
  (is (approx 8.8184976808 (weight-conversion "kilogram" "pound" 4)))
  (is (approx 0.7873658640000001 (weight-conversion "kilogram" "stone" 5)))
  (is (approx 141.095962892 (weight-conversion "kilogram" "ounce" 4)))
  (is (= 15000 (weight-conversion "kilogram" "carrat" 3)))
  (is (= 6.022136652e26 (weight-conversion "kilogram" "atomic-mass-unit" 1)))
  (is (= 0.001 (weight-conversion "gram" "kilogram" 1)))
  (is (= 3.0 (weight-conversion "gram" "gram" 3)))
  (is (= 2000.0 (weight-conversion "gram" "milligram" 2)))
  (is (thrown? clojure.lang.ExceptionInfo
               (weight-conversion "kilogram" "wrongunit" 1))))
