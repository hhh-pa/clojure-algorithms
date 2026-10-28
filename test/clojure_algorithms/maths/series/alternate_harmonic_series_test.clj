(ns clojure-algorithms.maths.series.alternate-harmonic-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.alternate-harmonic-series :as sut]))

(deftest test-fn-is-alternate-harmonic-series-examples
  (is (= true (boolean (sut/is-alternate-harmonic-series [1 -0.5 0.3333333333333333 -0.25 0.2]))))
  (is (= true (boolean (sut/is-alternate-harmonic-series [0.4 -0.2 0.13333333333333333 -0.1 0.08]))))
  (is (= false (boolean (sut/is-alternate-harmonic-series [1 0.5 0.3333333333333333 0.25]))))
  (is (= false (boolean (sut/is-alternate-harmonic-series [1 -0.5 0.25 -0.125]))))
  (is (thrown? Exception (sut/is-alternate-harmonic-series 4)))
  (is (thrown? Exception (sut/is-alternate-harmonic-series [])))
  (is (thrown? Exception (sut/is-alternate-harmonic-series [0])))
  (is (thrown? Exception (sut/is-alternate-harmonic-series [1 2 0 6]))))
