(ns clojure-algorithms.maths.special-numbers.polygonal-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.polygonal-numbers :as sut]))

(deftest test-fn-polygonal-num-examples
  (is (approx 0 (sut/polygonal-num 0 3)))
  (is (approx 6 (sut/polygonal-num 3 3)))
  (is (approx 25 (sut/polygonal-num 5 4)))
  (is (approx 5 (sut/polygonal-num 2 5)))
  (is (thrown? Exception (sut/polygonal-num -1 0)))
  (is (thrown? Exception (sut/polygonal-num 0 2))))
