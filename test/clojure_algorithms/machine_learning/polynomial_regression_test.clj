(ns clojure-algorithms.machine-learning.polynomial-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.polynomial-regression :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main))
  (is (some? #'sut/make-PolynomialRegression))
  (is (some? #'sut/PolynomialRegression-design-matrix))
  (is (some? #'sut/PolynomialRegression-fit))
  (is (some? #'sut/PolynomialRegression-predict))
  (is (some? #'sut/PolynomialRegression---slots--)))
