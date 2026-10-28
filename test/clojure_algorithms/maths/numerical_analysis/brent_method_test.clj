(ns clojure-algorithms.maths.numerical-analysis.brent-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.brent-method :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/brent-method)))
