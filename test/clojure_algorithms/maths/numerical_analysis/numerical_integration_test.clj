(ns clojure-algorithms.maths.numerical-analysis.numerical-integration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure-algorithms.maths.numerical-analysis.numerical-integration :as sut]))

(deftest test-fn-trapezoidal-area-examples
  (is (= "10.000"
         (format "%.3f" (sut/trapezoidal-area (fn [x] 5) 12.0 14.0 1000))))
  (is (= "192.0000"
         (format "%.4f"
                 (sut/trapezoidal-area (fn [x] (* 9 x x)) -4.0 0 10000))))
  (is (= "384.0000"
         (format "%.4f"
                 (sut/trapezoidal-area (fn [x] (* 9 x x)) -4.0 4.0 10000)))))

(deftest test-trapezoidal-area-default-steps
  (is (number? (sut/trapezoidal-area (fn [x] (* x x x)) 0.0 1.0))))
