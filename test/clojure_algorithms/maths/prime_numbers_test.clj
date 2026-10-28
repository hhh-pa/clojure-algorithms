(ns clojure-algorithms.maths.prime-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.prime-numbers :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/slow-primes))
  (is (some? #'sut/primes))
  (is (some? #'sut/fast-primes))
  (is (some? #'sut/benchmark)))
