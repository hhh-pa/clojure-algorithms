(ns clojure-algorithms.maths.fermat-little-theorem-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.fermat-little-theorem :as sut]))

(deftest test-fn-binary-exponentiation-examples
  (is (approx 4 (sut/binary-exponentiation 2 10 17)))
  (is (approx 1 (sut/binary-exponentiation 3 0 5)))
  (is (approx 8 (sut/binary-exponentiation 5 3 13))))
