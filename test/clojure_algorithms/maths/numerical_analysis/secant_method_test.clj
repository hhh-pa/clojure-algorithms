(ns clojure-algorithms.maths.numerical-analysis.secant-method-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.secant-method :as sut]))

(deftest test-fn-f-examples
  (is (approx 39.98652410600183 (sut/f 5))))

(deftest test-fn-secant-method-examples
  (is (approx 0.2139409276214589 (sut/secant-method 1 3 2))))
