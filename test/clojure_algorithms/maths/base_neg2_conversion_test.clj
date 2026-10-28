(ns clojure-algorithms.maths.base-neg2-conversion-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.base-neg2-conversion :as sut]))

(deftest test-fn-decimal-to-negative-base-2-examples
  (is (approx 0 (sut/decimal-to-negative-base-2 0)))
  (is (approx 111101 (sut/decimal-to-negative-base-2 -19)))
  (is (approx 100 (sut/decimal-to-negative-base-2 4)))
  (is (approx 11011 (sut/decimal-to-negative-base-2 7))))

(deftest test-fn-decimal-to-negative-base-2-extra
  (is (approx 1 (sut/decimal-to-negative-base-2 1)))
  (is (approx 110 (sut/decimal-to-negative-base-2 2)))
  (is (approx 10 (sut/decimal-to-negative-base-2 -2))))
