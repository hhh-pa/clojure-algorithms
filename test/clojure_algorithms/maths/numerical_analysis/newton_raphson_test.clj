(ns clojure-algorithms.maths.numerical-analysis.newton-raphson-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.newton-raphson :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/calc-derivative))
  (is (some? #'sut/newton-raphson))
  (is (some? #'sut/RealFunc)))
