(ns clojure-algorithms.maths.modular-exponential-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.modular-exponential :as sut]))

(deftest test-fn-modular-exponential-examples
  (is (approx 1 (sut/modular-exponential 5 0 10)))
  (is (approx 4 (sut/modular-exponential 2 8 7)))
  (is (approx -1 (sut/modular-exponential 3 -2 9))))
