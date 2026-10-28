(ns clojure-algorithms.maths.is-square-free-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-square-free :as sut]))

(deftest test-fn-is-square-free-examples
  (is (= false (boolean (sut/is-square-free [1 1 2 3 4]))))
  (is (= true (boolean (sut/is-square-free [1 3 4 "sd" 0.0]))))
  (is (= true (boolean (sut/is-square-free [1 0.5 2 0.0]))))
  (is (= false (boolean (sut/is-square-free [1 2 2 5]))))
  (is (= true (boolean (sut/is-square-free "asd"))))
  (is (thrown? Exception (sut/is-square-free 24))))
