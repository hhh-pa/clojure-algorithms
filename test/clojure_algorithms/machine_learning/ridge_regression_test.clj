(ns clojure-algorithms.machine-learning.ridge-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.ridge-regression :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-RidgeRegression))
  (is (some? #'sut/RidgeRegression-feature-scaling))
  (is (some? #'sut/RidgeRegression-fit))
  (is (some? #'sut/RidgeRegression-predict))
  (is (some? #'sut/RidgeRegression-compute-cost))
  (is (some? #'sut/RidgeRegression-mean-absolute-error)))
