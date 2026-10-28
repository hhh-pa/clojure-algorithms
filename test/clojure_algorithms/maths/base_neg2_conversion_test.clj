(ns clojure-algorithms.maths.base-neg2-conversion-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.base-neg2-conversion :as sut]))

(deftest test-fn-decimal-to-negative-base-2-examples
  (is (approx 0 (sut/decimal-to-negative-base-2 0)))
  (is (approx 111101 (sut/decimal-to-negative-base-2 -19)))
  (is (approx 100 (sut/decimal-to-negative-base-2 4)))
  (is (approx 11011 (sut/decimal-to-negative-base-2 7))))
