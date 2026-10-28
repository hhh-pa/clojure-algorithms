(ns clojure-algorithms.machine-learning.linear-discriminant-analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.linear-discriminant-analysis :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/gaussian-distribution))
  (is (some? #'sut/y-generator))
  (is (some? #'sut/calculate-mean))
  (is (some? #'sut/calculate-probabilities))
  (is (some? #'sut/calculate-variance))
  (is (some? #'sut/predict-y-values))
  (is (some? #'sut/accuracy))
  (is (some? #'sut/valid-input))
  (is (some? #'sut/main)))
