(ns clojure-algorithms.machine-learning.gaussian-naive-bayes-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.gaussian-naive-bayes :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main)))
