(ns clojure-algorithms.machine-learning.naive-bayes-text-classification-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.naive-bayes-text-classification :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/build-toy-dataset))
  (is (some? #'sut/make-NaiveBayesTextClassifier))
  (is (some? #'sut/NaiveBayesTextClassifier-tokenize))
  (is (some? #'sut/NaiveBayesTextClassifier-fit))
  (is (some? #'sut/NaiveBayesTextClassifier-predict-proba))
  (is (some? #'sut/NaiveBayesTextClassifier-predict)))
