(ns clojure-algorithms.maths.recursive-digit-sum-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.recursive-digit-sum :as sut]))

(deftest test-fn-super-digit-examples
  (is (approx 3 (sut/super-digit "148" 3)))
  (is (approx 8 (sut/super-digit "9875" 4)))
  (is (approx 9 (sut/super-digit "123" 3))))
