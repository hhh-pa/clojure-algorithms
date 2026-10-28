(ns clojure-algorithms.maths.pollard-rho-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.pollard-rho :as sut]))

(deftest test-fn-pollard-rho-examples
  (is (approx 274177 (sut/pollard-rho 18446744073709551617)))
  (is (approx 9876543191 (sut/pollard-rho 97546105601219326301)))
  (is (approx 2 (sut/pollard-rho 100)))
  (is (nil? (sut/pollard-rho 17)))
  (is (approx 17 (sut/pollard-rho 4913)))
  (is (approx 21 (sut/pollard-rho 105)))
  (is (thrown? Exception (sut/pollard-rho 1))))
