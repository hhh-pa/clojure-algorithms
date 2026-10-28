(ns clojure-algorithms.machine-learning.ordinary-least-squares-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.ordinary-least-squares-regression :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/ols-regression)))
