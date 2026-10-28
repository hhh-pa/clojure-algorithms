(ns clojure-algorithms.maths.quadratic-equations-complex-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.quadratic-equations-complex-numbers :as sut]))

(deftest test-fn-quadratic-roots-examples
  (is (thrown? Exception (sut/quadratic-roots 5 6 1)))
  (is (thrown? Exception (sut/quadratic-roots 1 -6 25))))
