(ns clojure-algorithms.maths.binomial-coefficient-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.binomial-coefficient :as sut]))

(deftest test-fn-binomial-coefficient-examples
  (is (approx 252 (sut/binomial-coefficient 10 5)))
  (is (approx 1 (sut/binomial-coefficient 10 0)))
  (is (approx 1 (sut/binomial-coefficient 0 10)))
  (is (approx 1 (sut/binomial-coefficient 10 10)))
  (is (approx 10 (sut/binomial-coefficient 5 2)))
  (is (approx 0 (sut/binomial-coefficient 5 6)))
  (is (approx 0 (sut/binomial-coefficient 3 5)))
  (is (thrown? Exception (sut/binomial-coefficient -2 3)))
  (is (thrown? Exception (sut/binomial-coefficient 5 -1)))
  (is (thrown? Exception (sut/binomial-coefficient 10 5.1))))
