(ns clojure-algorithms.maths.trapezoidal-rule-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.trapezoidal-rule :as sut]))

(deftest test-fn-f-examples
  (is (approx 0 (sut/f 0)))
  (is (approx 1 (sut/f 1)))
  (is (approx 0.25 (sut/f 0.5))))

(deftest test-fn-main-examples
  (is (= "y = 0.3349999999999999\n" (with-out-str (sut/main)))))
