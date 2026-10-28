(ns clojure-algorithms.project-euler.problem-123.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-123.sol1 :refer [sieve
                                                                       solution]]))

(deftest sieve-test
  (is (= [2 3 5 7 11 13] (take 6 (sieve)))))

(deftest solution-test
  (is (= 2371 (solution 1e8)))
  (is (= 7037 (solution 1e9))))
