(ns clojure-algorithms.project-euler.problem-095.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-095.sol1 :refer [find-longest-chain
                                                                       generate-primes
                                                                       multiply
                                                                       solution]]))

(deftest generate-primes-test
  (is (= [2 3 5] (generate-primes 6))))

(deftest multiply-test
  (let [chain (atom [0 0 0])]
    (is (= {2 1} (multiply chain [2] 0 1 2 0 {})))
    (is (= [0 0 1] @chain))))

(deftest find-longest-chain-test
  (is (= 6 (find-longest-chain [0 0 0 0 0 0 6] 6))))

(deftest solution-test
  (is (= 6 (solution 10))))
