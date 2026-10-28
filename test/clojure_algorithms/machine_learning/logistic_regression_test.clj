(ns clojure-algorithms.machine-learning.logistic-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.logistic-regression :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/sigmoid-function))
  (is (some? #'sut/cost-function))
  (is (some? #'sut/log-likelihood))
  (is (some? #'sut/logistic-reg)))
