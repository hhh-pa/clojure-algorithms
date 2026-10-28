(ns clojure-algorithms.maths.monte-carlo-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.monte-carlo :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/pi-estimator))
  (is (some? #'sut/area-under-curve-estimator))
  (is (some? #'sut/area-under-line-estimator-check))
  (is (some? #'sut/pi-estimator-using-area-under-curve)))
