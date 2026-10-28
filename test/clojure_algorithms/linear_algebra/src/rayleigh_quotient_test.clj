(ns clojure-algorithms.linear-algebra.src.rayleigh-quotient-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.rayleigh-quotient
             :refer [hermitian? rayleigh-quotient]]))

(deftest test-rayleigh-quotient
  (is (true? (hermitian? [[1 2 4] [2 3 -1] [4 -1 1]])))
  (is (false? (hermitian? [[1 2] [3 4]])))
  (is (= 3.0 (rayleigh-quotient [[1 2 4] [2 3 -1] [4 -1 1]] [1 2 3]))))
