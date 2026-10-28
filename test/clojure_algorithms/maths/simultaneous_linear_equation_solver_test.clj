(ns clojure-algorithms.maths.simultaneous-linear-equation-solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.simultaneous-linear-equation-solver :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/simplify))
  (is (some? #'sut/solve-simultaneous)))
