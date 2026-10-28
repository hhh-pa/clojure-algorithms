(ns clojure-algorithms.maths.series.harmonic-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.harmonic-series :as sut]))

(deftest test-fn-harmonic-series-examples
  (is (approx ["1" "1/2" "1/3" "1/4" "1/5"] (sut/harmonic-series 5)))
  (is (approx ["1" "1/2" "1/3" "1/4" "1/5"] (sut/harmonic-series 5.0)))
  (is (approx ["1" "1/2" "1/3" "1/4" "1/5"] (sut/harmonic-series 5.1)))
  (is (approx [] (sut/harmonic-series -5)))
  (is (approx [] (sut/harmonic-series 0)))
  (is (approx ["1"] (sut/harmonic-series 1))))
