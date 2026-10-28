(ns clojure-algorithms.maths.sum-of-digits-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sum-of-digits :as sut]))

(deftest test-fn-sum-of-digits-examples
  (is (approx 15 (sut/sum-of-digits 12345)))
  (is (approx 6 (sut/sum-of-digits 123)))
  (is (approx 6 (sut/sum-of-digits -123)))
  (is (approx 0 (sut/sum-of-digits 0))))

(deftest test-fn-sum-of-digits-recursion-examples
  (is (approx 15 (sut/sum-of-digits-recursion 12345)))
  (is (approx 6 (sut/sum-of-digits-recursion 123)))
  (is (approx 6 (sut/sum-of-digits-recursion -123)))
  (is (approx 0 (sut/sum-of-digits-recursion 0))))

(deftest test-fn-sum-of-digits-compact-examples
  (is (approx 15 (sut/sum-of-digits-compact 12345)))
  (is (approx 6 (sut/sum-of-digits-compact 123)))
  (is (approx 6 (sut/sum-of-digits-compact -123)))
  (is (approx 0 (sut/sum-of-digits-compact 0))))
