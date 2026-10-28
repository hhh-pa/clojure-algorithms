(ns clojure-algorithms.maths.series.geometric-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.geometric :as sut]))

(deftest test-fn-is-geometric-series-examples
  (is (= true (boolean (sut/is-geometric-series [2 4 8]))))
  (is (= true (boolean (sut/is-geometric-series [3 6 12 24]))))
  (is (= false (boolean (sut/is-geometric-series [1 2 3]))))
  (is (= false (boolean (sut/is-geometric-series [0 0 3]))))
  (is (thrown? Exception (sut/is-geometric-series [])))
  (is (thrown? Exception (sut/is-geometric-series 4))))

(deftest test-fn-geometric-mean-examples
  (is (approx 3.9999999999999996 (sut/geometric-mean [2 4 8])))
  (is (approx 8.48528137423857 (sut/geometric-mean [3 6 12 24])))
  (is (approx 7.999999999999999 (sut/geometric-mean [4 8 16])))
  (is (thrown? Exception (sut/geometric-mean 4)))
  (is (approx 1.8171205928321397 (sut/geometric-mean [1 2 3])))
  (is (approx 0.0 (sut/geometric-mean [0 2 3])))
  (is (thrown? Exception (sut/geometric-mean []))))
