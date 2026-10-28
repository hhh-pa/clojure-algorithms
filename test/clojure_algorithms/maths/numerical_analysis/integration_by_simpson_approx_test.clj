(ns clojure-algorithms.maths.numerical-analysis.integration-by-simpson-approx-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.integration-by-simpson-approx :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/f))
  (is (some? #'sut/simpson-integration))
  (is (some? #'sut/N-STEPS)))
