(ns clojure-algorithms.machine-learning.forecasting.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.forecasting.run :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/linear-regression-prediction))
  (is (some? #'sut/sarimax-predictor))
  (is (some? #'sut/support-vector-regressor))
  (is (some? #'sut/random-forest-regressor))
  (is (some? #'sut/interquartile-range-checker))
  (is (some? #'sut/data-safety-checker)))
