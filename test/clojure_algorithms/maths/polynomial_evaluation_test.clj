(ns clojure-algorithms.maths.polynomial-evaluation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.polynomial-evaluation :as sut]))

(deftest test-fn-evaluate-poly-examples
  (is (approx 79800.0 (sut/evaluate-poly [0.0 0.0 5.0 9.3 7.0] 10.0))))

(deftest test-fn-horner-examples
  (is (approx 79800.0 (sut/horner [0.0 0.0 5.0 9.3 7.0] 10.0))))
