(ns clojure-algorithms.maths.numerical-analysis.simpson-rule-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.simpson-rule :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/method-2))
  (is (some? #'sut/make-points))
  (is (some? #'sut/f))
  (is (some? #'sut/main)))
