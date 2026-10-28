(ns clojure-algorithms.maths.padovan-sequence-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.padovan-sequence :as sut]))

(deftest test-fn-padovan-sequence-examples
  (is (approx 1 (sut/padovan-sequence 0)))
  (is (approx 1 (sut/padovan-sequence 1)))
  (is (approx 1 (sut/padovan-sequence 2)))
  (is (approx 2 (sut/padovan-sequence 3)))
  (is (approx 2 (sut/padovan-sequence 4)))
  (is (approx 3 (sut/padovan-sequence 5)))
  (is (approx 12 (sut/padovan-sequence 10)))
  (is (thrown? Exception (sut/padovan-sequence -1))))
