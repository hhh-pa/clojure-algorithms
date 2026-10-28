(ns clojure-algorithms.maths.numerical-analysis.bisection-2-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.bisection-2 :as sut]))

(deftest test-fn-equation-examples
  (is (approx -15 (sut/equation 5)))
  (is (approx 10 (sut/equation 0)))
  (is (approx -15 (sut/equation -5)))
  (is (approx 9.99 (sut/equation 0.1)))
  (is (approx 9.99 (sut/equation -0.1))))

(deftest test-fn-bisection-examples
  (is (approx 3.1611328125 (sut/bisection -2 5)))
  (is (approx 3.158203125 (sut/bisection 0 6)))
  (is (thrown? Exception (sut/bisection 2 3))))
