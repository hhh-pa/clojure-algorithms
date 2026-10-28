(ns clojure-algorithms.machine-learning.linear-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.linear-regression :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/collect-dataset))
  (is (some? #'sut/run-steep-gradient-descent))
  (is (some? #'sut/sum-of-square-error))
  (is (some? #'sut/run-linear-regression))
  (is (some? #'sut/mean-absolute-error))
  (is (some? #'sut/plot-regression))
  (is (some? #'sut/plot-loss))
  (is (some? #'sut/main)))
