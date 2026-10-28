(ns clojure-algorithms.maths.eulers-totient-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.eulers-totient :as sut]))

(deftest test-fn-totient-examples
  ;; Upstream doctest: "i has totient_calculation[i] relative primes."
  (is (= [-1 0 1 2 2 4 2 6 4 6 9] (sut/totient 10)))
  (is (= [0 1 2 2 4 2 6 4 6] (subvec (sut/totient 10) 1 10)))
  (is (approx [-1 0 1] (sut/totient 2))))
