(ns clojure-algorithms.maths.pi-monte-carlo-estimation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.pi-monte-carlo-estimation :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/estimate-pi))
  (is (some? #'sut/make-Point))
  (is (some? #'sut/Point-is-in-unit-circle))
  (is (some? #'sut/Point-random-unit-square)))
