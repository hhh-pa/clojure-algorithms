(ns clojure-algorithms.maths.numerical-analysis.newton-forward-interpolation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.newton-forward-interpolation :as sut]))

(deftest test-fn-ucal-examples
  (is (approx 0 (sut/ucal 1 2)))
  (is (approx 0.11000000000000011 (sut/ucal 1.1 2)))
  (is (approx 0.23999999999999994 (sut/ucal 1.2 2))))
