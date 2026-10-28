(ns clojure-algorithms.maths.numerical-analysis.intersection-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.intersection :as sut]))

(deftest test-fn-f-examples
  (is (approx -1.0 (sut/f 2))))
