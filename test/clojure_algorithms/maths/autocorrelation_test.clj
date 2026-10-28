(ns clojure-algorithms.maths.autocorrelation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.autocorrelation :as sut]))

(deftest test-fn-autocorrelation-examples
  (is (thrown? Exception (sut/autocorrelation [1 2 3] 5))))
