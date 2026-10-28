(ns clojure-algorithms.machine-learning.data-transformations-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.data-transformations :as sut]))

(deftest test-fn-normalization-examples
  (is (approx [0.0 0.104 0.167 0.375 0.583 1.0] (sut/normalization [2 7 10 20 30 50])))
  (is (approx [0.0 0.25 0.5 0.75 1.0] (sut/normalization [5 10 15 20 25]))))

(deftest test-fn-standardization-examples
  (is (approx [-0.999 -0.719 -0.551 0.009 0.57 1.69] (sut/standardization [2 7 10 20 30 50])))
  (is (approx [-1.265 -0.632 0.0 0.632 1.265] (sut/standardization [5 10 15 20 25]))))
