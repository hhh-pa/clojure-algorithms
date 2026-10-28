(ns clojure-algorithms.maths.numerical-analysis.runge-kutta-gills-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.runge-kutta-gills :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/runge-kutta-gills)))
