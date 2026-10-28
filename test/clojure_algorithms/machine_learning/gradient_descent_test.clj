(ns clojure-algorithms.machine-learning.gradient-descent-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.gradient-descent :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/-error))
  (is (some? #'sut/-hypothesis-value))
  (is (some? #'sut/output))
  (is (some? #'sut/calculate-hypothesis-value))
  (is (some? #'sut/summation-of-cost-derivative))
  (is (some? #'sut/get-cost-derivative))
  (is (some? #'sut/run-gradient-descent))
  (is (some? #'sut/test-gradient-descent))
  (is (some? #'sut/train-data))
  (is (some? #'sut/test-data))
  (is (some? #'sut/parameter-vector))
  (is (some? #'sut/m))
  (is (some? #'sut/LEARNING-RATE)))
