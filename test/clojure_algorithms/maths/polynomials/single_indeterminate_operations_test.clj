(ns clojure-algorithms.maths.polynomials.single-indeterminate-operations-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.polynomials.single-indeterminate-operations :as sut]))

(deftest test-fn-repr-examples
  (is (some? (let [p (sut/make-Polynomial 2 [1 2 3])] p))))

(deftest test-fn-eq-examples
  (is (let [p (sut/make-Polynomial 2 [1 2 3]) q (sut/make-Polynomial 2 [1 2 3])] (approx p q))))

(deftest test-fn-ne-examples
  (is (let [p (sut/make-Polynomial 2 [1 2 3]) q (sut/make-Polynomial 2 [1 2 3])] (approx p q))))
