(ns clojure-algorithms.maths.numerical-analysis.runge-kutta-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.runge-kutta :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/runge-kutta)))
