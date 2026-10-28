(ns clojure-algorithms.maths.sum-of-harmonic-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.sum-of-harmonic-series :as sut]))

(deftest test-fn-sum-of-harmonic-progression-examples
  (is (approx 0.75 (sut/sum-of-harmonic-progression 0.5 2 2)))
  (is (approx 0.45666666666666667 (sut/sum-of-harmonic-progression 0.2 5 5))))

(deftest test-fn-sum-of-harmonic-progression-extra
  (is (approx 1.0 (sut/sum-of-harmonic-progression 1.0 1.0 1)))
  (is (approx 1.5 (sut/sum-of-harmonic-progression 1.0 1.0 2))))
