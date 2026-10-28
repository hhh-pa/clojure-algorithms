(ns clojure-algorithms.maths.series.geometric-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.geometric-series :as sut]))

(deftest test-fn-geometric-series-examples
  (is (approx [2 4.0 8.0 16.0] (sut/geometric-series 4 2 2)))
  (is (approx [2.0 4.0 8.0 16.0] (sut/geometric-series 4.0 2.0 2.0)))
  (is (approx [2.1 4.41 9.261000000000001 19.448100000000004] (sut/geometric-series 4.1 2.1 2.1)))
  (is (approx [2 -4.0 8.0 -16.0] (sut/geometric-series 4 2 -2)))
  (is (approx [-2 -4.0 -8.0 -16.0] (sut/geometric-series 4 -2 2)))
  (is (approx [] (sut/geometric-series -4 2 2)))
  (is (approx [] (sut/geometric-series 0 100 500)))
  (is (approx [1] (sut/geometric-series 1 1 1)))
  (is (approx [] (sut/geometric-series 0 0 0))))
