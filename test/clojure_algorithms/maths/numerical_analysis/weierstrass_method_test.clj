(ns clojure-algorithms.maths.numerical-analysis.weierstrass-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.weierstrass-method :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/weierstrass-method)))
