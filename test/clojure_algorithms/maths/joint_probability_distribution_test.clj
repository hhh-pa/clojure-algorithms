(ns clojure-algorithms.maths.joint-probability-distribution-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.joint-probability-distribution :as sut]))

(deftest test-fn-covariance-examples
  (is (approx -5.551115123125783e-17 (sut/covariance [1 2] [-2 5 8] [0.7 0.3] [0.3 0.5 0.2]))))

(deftest test-fn-standard-deviation-examples
  (is (approx 0.458257569495584 (sut/standard-deviation 0.21))))
