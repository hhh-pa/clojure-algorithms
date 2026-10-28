(ns clojure-algorithms.project-euler.problem-124.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-124.sol1 :refer [generate-n
                                                                       generate-primes
                                                                       generate-rads
                                                                       solution]]))

(deftest generate-primes-test
  (is (= [2 3 5] (generate-primes 6))))

(deftest generate-n-test
  (is (= #{1 2 4 8} (generate-n [2] 10 1))))

(deftest generate-rads-test
  (is (= {2 [2]} (generate-rads [2] 10 1 []))))

(deftest solution-test
  (is (= 9 (solution 10 6)))
  (is (= 7 (solution 10 9))))
