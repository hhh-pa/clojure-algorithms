(ns clojure-algorithms.maths.numerical-analysis.proper-fractions-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.proper-fractions :as sut]))

(deftest test-fn-proper-fractions-examples
  (is (thrown? Exception (sut/proper-fractions -15)))
  (is (thrown? Exception (sut/proper-fractions 1.2))))
