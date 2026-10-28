(ns clojure-algorithms.maths.numerical-analysis.runge-kutta-fehlberg-45-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.runge-kutta-fehlberg-45 :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/runge-kutta-fehlberg-45)))
