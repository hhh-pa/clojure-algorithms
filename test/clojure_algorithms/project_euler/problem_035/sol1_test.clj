(ns clojure-algorithms.project-euler.problem-035.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-035.sol1 :refer [prime? contains-an-even-digit? find-circular-primes]]))

(deftest prime?-test
  (is (false? (prime? 87)))
  (is (true? (prime? 23)))
  (is (false? (prime? 25363))))

(deftest contains-an-even-digit?-test
  (is (true? (contains-an-even-digit? 0)))
  (is (false? (contains-an-even-digit? 975317933)))
  (is (true? (contains-an-even-digit? -245679))))

(deftest find-circular-primes-test
  (is (= 13 (count (find-circular-primes 100)))))
