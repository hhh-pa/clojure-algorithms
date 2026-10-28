(ns clojure-algorithms.maths.polynomial-evaluation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.polynomial-evaluation :as sut]))

(def ^:private poly [0.0 0.0 5.0 9.3 7.0])

(deftest test-fn-evaluate-poly-examples
  (is (approx 79800.0 (sut/evaluate-poly poly 10.0)))
  ;; Upstream __main__ example: f(-13) = 180339.9
  (is (approx 180339.9 (sut/evaluate-poly poly -13.0))))

(deftest test-fn-horner-examples
  (is (approx 79800.0 (sut/horner poly 10.0)))
  (is (approx 180339.9 (sut/horner poly -13.0))))
