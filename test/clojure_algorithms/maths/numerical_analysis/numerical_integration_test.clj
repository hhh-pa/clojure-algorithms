(ns clojure-algorithms.maths.numerical-analysis.numerical-integration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.numerical-integration :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/trapezoidal-area)))
