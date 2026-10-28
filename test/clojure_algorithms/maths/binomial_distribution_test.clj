(ns clojure-algorithms.maths.binomial-distribution-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.binomial-distribution :as sut]))

(deftest test-fn-binomial-distribution-examples
  (is (approx 0.30870000000000003 (sut/binomial-distribution 3 5 0.7)))
  (is (approx 0.375 (sut/binomial-distribution 2 4 0.5))))
