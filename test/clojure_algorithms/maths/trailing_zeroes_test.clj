(ns clojure-algorithms.maths.trailing-zeroes-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.trailing-zeroes :as sut]))

(deftest test-fn-trailing-zeroes-examples
  (is (approx 3 (sut/trailing-zeroes 1000)))
  (is (approx 5 (sut/trailing-zeroes 102983100000)))
  (is (approx 1 (sut/trailing-zeroes 0)))
  (is (approx 0 (sut/trailing-zeroes 913273)))
  (is (approx -1 (sut/trailing-zeroes -10))))
