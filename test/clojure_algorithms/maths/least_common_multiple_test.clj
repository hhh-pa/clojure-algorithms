(ns clojure-algorithms.maths.least-common-multiple-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.least-common-multiple :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/least-common-multiple-slow))
  (is (some? #'sut/least-common-multiple-fast))
  (is (some? #'sut/benchmark)))
