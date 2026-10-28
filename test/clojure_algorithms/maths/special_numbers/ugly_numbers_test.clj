(ns clojure-algorithms.maths.special-numbers.ugly-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.ugly-numbers :as sut]))

(deftest test-fn-ugly-numbers-examples
  (is (approx 1536 (sut/ugly-numbers 100)))
  (is (approx 1 (sut/ugly-numbers 0)))
  (is (approx 36 (sut/ugly-numbers 20)))
  (is (approx 1 (sut/ugly-numbers -5))))
