(ns clojure-algorithms.machine-learning.loss-functions-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.loss-functions :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/binary-cross-entropy))
  (is (some? #'sut/binary-focal-cross-entropy))
  (is (some? #'sut/categorical-cross-entropy))
  (is (some? #'sut/sparse-categorical-cross-entropy))
  (is (some? #'sut/categorical-focal-cross-entropy))
  (is (some? #'sut/gaussian-negative-log-likelihood-loss))
  (is (some? #'sut/hinge-loss))
  (is (some? #'sut/huber-loss))
  (is (some? #'sut/mean-squared-error))
  (is (some? #'sut/mean-absolute-error))
  (is (some? #'sut/mean-squared-logarithmic-error))
  (is (some? #'sut/mean-absolute-percentage-error))
  (is (some? #'sut/perplexity-loss))
  (is (some? #'sut/smooth-l1-loss))
  (is (some? #'sut/kullback-leibler-divergence))
  (is (some? #'sut/connectionist-temporal-classification-loss))
  (is (some? #'sut/symmetric-mean-absolute-percentage-error)))
