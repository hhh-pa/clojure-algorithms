(ns clojure-algorithms.machine-learning.gradient-boosting-regressor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.gradient-boosting-regressor :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main)))
