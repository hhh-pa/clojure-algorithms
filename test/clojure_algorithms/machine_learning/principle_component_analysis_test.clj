(ns clojure-algorithms.machine-learning.principle-component-analysis-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.principle-component-analysis :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/collect-dataset))
  (is (some? #'sut/apply-pca))
  (is (some? #'sut/main)))
