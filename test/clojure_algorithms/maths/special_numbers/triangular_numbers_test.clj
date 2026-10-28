(ns clojure-algorithms.maths.special-numbers.triangular-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.triangular-numbers :as sut]))

(deftest test-fn-triangular-number-examples
  (is (approx 1 (sut/triangular-number 1)))
  (is (approx 6 (sut/triangular-number 3)))
  (is (thrown? Exception (sut/triangular-number -1))))
