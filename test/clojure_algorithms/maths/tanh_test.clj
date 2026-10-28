(ns clojure-algorithms.maths.tanh-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.tanh :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/tangent-hyperbolic)))
