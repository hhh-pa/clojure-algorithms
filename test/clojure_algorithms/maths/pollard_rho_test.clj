(ns clojure-algorithms.maths.pollard-rho-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.pollard-rho :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-pollard-rho-examples
  (is (approx 274177 (sut/pollard-rho 18446744073709551617N)))
  (is (approx 9876543191 (sut/pollard-rho 97546105601219326301N)))
  (is (approx 2 (sut/pollard-rho 100)))
  (is (nil? (sut/pollard-rho 17)))
  (is (approx 17 (sut/pollard-rho 4913)))
  (is (nil? (sut/pollard-rho 4913 2 1 1)))
  (is (approx 21 (sut/pollard-rho 105)))
  (is (thrown? Exception (sut/pollard-rho 1)))
  (is (= :ValueError (err-type #(sut/pollard-rho 1))))
  (is (= :ValueError (err-type #(sut/pollard-rho 0)))))

(deftest test-pollard-rho-extra
  (let [f (sut/pollard-rho 8051)]
    (is (zero? (mod 8051 f))))
  (is (nil? (sut/pollard-rho 101)))
  (is (= 2 (sut/pollard-rho 1000000000000N))))
