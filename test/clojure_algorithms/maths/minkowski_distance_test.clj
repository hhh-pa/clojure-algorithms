(ns clojure-algorithms.maths.minkowski-distance-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.minkowski-distance :as sut]))

(deftest test-fn-minkowski-distance-examples
  (is (approx 2.0 (sut/minkowski-distance [1.0 1.0] [2.0 2.0] 1)))
  (is (approx 8.0 (sut/minkowski-distance [1.0 2.0 3.0 4.0] [5.0 6.0 7.0 8.0] 2))))
