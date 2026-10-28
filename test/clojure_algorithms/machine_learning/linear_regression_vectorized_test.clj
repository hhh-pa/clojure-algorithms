(ns clojure-algorithms.machine-learning.linear-regression-vectorized-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.linear-regression-vectorized :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/collect-dataset))
  (is (some? #'sut/gradient-descent))
  (is (some? #'sut/mean-absolute-error))
  (is (some? #'sut/main)))
