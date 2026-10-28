(ns clojure-algorithms.machine-learning.gradient-boosting-classifier-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.gradient-boosting-classifier :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-GradientBoostingClassifier))
  (is (some? #'sut/GradientBoostingClassifier-fit))
  (is (some? #'sut/GradientBoostingClassifier-predict))
  (is (some? #'sut/GradientBoostingClassifier-gradient)))
