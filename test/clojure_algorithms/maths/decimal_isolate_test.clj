(ns clojure-algorithms.maths.decimal-isolate-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.decimal-isolate :as sut]))

(deftest test-fn-decimal-isolate-examples
  (is (approx 0.53 (sut/decimal-isolate 1.53 0)))
  (is (approx 0.3 (sut/decimal-isolate 35.345 1)))
  (is (approx 0.34 (sut/decimal-isolate 35.345 2)))
  (is (approx 0.345 (sut/decimal-isolate 35.345 3)))
  (is (approx -0.789 (sut/decimal-isolate -14.789 3)))
  (is (approx 0 (sut/decimal-isolate 0 2)))
  (is (approx -0.1 (sut/decimal-isolate -14.123 1)))
  (is (approx -0.12 (sut/decimal-isolate -14.123 2)))
  (is (approx -0.123 (sut/decimal-isolate -14.123 3))))
