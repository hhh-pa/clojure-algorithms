(ns clojure-algorithms.maths.germain-primes-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.germain-primes :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/is-germain-prime))
  (is (some? #'sut/is-safe-prime)))
