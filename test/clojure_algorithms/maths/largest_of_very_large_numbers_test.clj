(ns clojure-algorithms.maths.largest-of-very-large-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.largest-of-very-large-numbers :as sut]))

(deftest test-fn-res-examples
  (is (approx 4.892790030352132 (sut/res 5 7)))
  (is (approx 0 (sut/res 0 5)))
  (is (approx 1 (sut/res 3 0))))
