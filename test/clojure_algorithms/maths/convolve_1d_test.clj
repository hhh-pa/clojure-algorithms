(ns clojure-algorithms.maths.convolve-1d-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.convolve-1d :as sut]))

(deftest test-class-signal-examples
  (is (some? (sut/make-Signal [1.0 3.0 2.0 -1.0]))))

(deftest test-class-discreteconvolve1d-examples
  (is (some? (let [s1 (sut/make-Signal [1 2 3 4 5]) s2 (sut/make-Signal [1 -1 2 -3])] (sut/make-DiscreteConvolve1D s1 s2)))))
