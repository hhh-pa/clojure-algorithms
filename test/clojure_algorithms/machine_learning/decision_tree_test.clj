(ns clojure-algorithms.machine-learning.decision-tree-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.decision-tree :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main))
  (is (some? #'sut/make-DecisionTree))
  (is (some? #'sut/DecisionTree-mean-squared-error))
  (is (some? #'sut/DecisionTree-train))
  (is (some? #'sut/DecisionTree-predict))
  (is (some? #'sut/make-TestDecisionTree))
  (is (some? #'sut/TestDecisionTree-helper-mean-squared-error-test)))
