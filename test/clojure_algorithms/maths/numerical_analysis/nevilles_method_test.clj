(ns clojure-algorithms.maths.numerical-analysis.nevilles-method-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.nevilles-method :as sut]))

(deftest test-fn-neville-interpolate-examples
  (is (thrown? Exception (sut/neville-interpolate [1 2 3 4 6] [6 7 8 9 11] ""))))
