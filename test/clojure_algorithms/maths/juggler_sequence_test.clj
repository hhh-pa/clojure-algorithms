(ns clojure-algorithms.maths.juggler-sequence-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.juggler-sequence :as sut]))

(deftest test-fn-juggler-sequence-examples
  (is (thrown? Exception (sut/juggler-sequence 0)))
  (is (approx [1] (sut/juggler-sequence 1)))
  (is (approx [2 1] (sut/juggler-sequence 2)))
  (is (approx [3 5 11 36 6 2 1] (sut/juggler-sequence 3)))
  (is (approx [5 11 36 6 2 1] (sut/juggler-sequence 5)))
  (is (approx [10 3 5 11 36 6 2 1] (sut/juggler-sequence 10)))
  (is (approx [25 125 1397 52214 228 15 58 7 18 4 2 1] (sut/juggler-sequence 25)))
  (is (thrown? Exception (sut/juggler-sequence 6.0)))
  (is (thrown? Exception (sut/juggler-sequence -1))))
