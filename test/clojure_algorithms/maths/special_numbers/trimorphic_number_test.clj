(ns clojure-algorithms.maths.special-numbers.trimorphic-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.trimorphic-number :as sut]))

(deftest test-fn-is-trimorphic-examples
  (is (= true (boolean (sut/is-trimorphic 0))))
  (is (= true (boolean (sut/is-trimorphic 1))))
  (is (= true (boolean (sut/is-trimorphic 4))))
  (is (= true (boolean (sut/is-trimorphic 5))))
  (is (= true (boolean (sut/is-trimorphic 24))))
  (is (= true (boolean (sut/is-trimorphic 249))))
  (is (= false (boolean (sut/is-trimorphic 2))))
  (is (= false (boolean (sut/is-trimorphic 7))))
  (is (= false (boolean (sut/is-trimorphic 10))))
  (is (thrown? Exception (sut/is-trimorphic -1))))
