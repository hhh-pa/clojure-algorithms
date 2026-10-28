(ns clojure-algorithms.maths.special-numbers.catalan-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.catalan-number :as sut]))

(deftest test-fn-catalan-examples
  (is (approx 14 (sut/catalan 5)))
  (is (thrown? Exception (sut/catalan 0)))
  (is (thrown? Exception (sut/catalan -1)))
  (is (thrown? Exception (sut/catalan 5.0))))
