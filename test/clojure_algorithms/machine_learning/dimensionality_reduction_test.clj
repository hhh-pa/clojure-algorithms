(ns clojure-algorithms.machine-learning.dimensionality-reduction-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.dimensionality-reduction :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/column-reshape))
  (is (some? #'sut/covariance-within-classes))
  (is (some? #'sut/covariance-between-classes))
  (is (some? #'sut/principal-component-analysis))
  (is (some? #'sut/linear-discriminant-analysis))
  (is (some? #'sut/t-distributed-stochastic-neighbor-embedding))
  (is (some? #'sut/test-linear-discriminant-analysis))
  (is (some? #'sut/test-principal-component-analysis))
  (is (some? #'sut/test-t-distributed-stochastic-neighbor-embedding)))
