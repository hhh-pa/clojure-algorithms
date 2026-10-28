(ns clojure-algorithms.project-euler.problem-234.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-234.sol1 :refer [prime-sieve
                                                                       solution]]))

(deftest prime-sieve-test
  (is (= [2] (prime-sieve 3)))
  (is (= [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47] (prime-sieve 50))))

(deftest solution-test
  (is (= 34825 (solution 1000)))
  (is (= 1134942 (solution 10000)))
  (is (= 36393008 (solution 100000))))
