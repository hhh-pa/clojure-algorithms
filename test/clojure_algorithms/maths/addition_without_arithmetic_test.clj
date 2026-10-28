(ns clojure-algorithms.maths.addition-without-arithmetic-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.addition-without-arithmetic :as sut]))

(deftest test-fn-add-examples
  (is (approx 8 (sut/add 3 5)))
  (is (approx 18 (sut/add 13 5)))
  (is (approx -5 (sut/add -7 2)))
  (is (approx -7 (sut/add 0 -7)))
  (is (approx -320 (sut/add -321 1))))
