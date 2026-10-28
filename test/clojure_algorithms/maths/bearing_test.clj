(ns clojure-algorithms.maths.bearing-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.bearing :as sut]))

(deftest test-fn-initial-bearing-examples
  (is (thrown? Exception (sut/initial-bearing [0.0 0.0] [0.0 0.0]))))
