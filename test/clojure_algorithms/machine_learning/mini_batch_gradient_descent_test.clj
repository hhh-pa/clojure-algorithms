(ns clojure-algorithms.machine-learning.mini-batch-gradient-descent-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.mini-batch-gradient-descent :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/mini-batch-gradient-descent)))
