(ns clojure-algorithms.maths.series.alternating-harmonic-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.alternating-harmonic-series :as sut]))

(deftest test-fn-is-alternate-harmonic-series-examples
  (is (= true (boolean (sut/is-alternate-harmonic-series [1 -0.5 0.3333333333333333 -0.25]))))
  (is (= false (boolean (sut/is-alternate-harmonic-series [1 0.5 0.3333333333333333]))))
  (is (= false (boolean (sut/is-alternate-harmonic-series [])))))
