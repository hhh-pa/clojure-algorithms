(ns clojure-algorithms.project-euler.problem-111.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-111.sol1 :refer [generate-primes
                                                                       is-prime
                                                                       solution]]))

(deftest generate-primes-test
  (is (= [2 3 5] (generate-primes 6))))

(deftest is-prime-test
  (is (true? (is-prime 5 [2 3]))))

(deftest solution-test
  (is (= 273700 (solution 10000))))
