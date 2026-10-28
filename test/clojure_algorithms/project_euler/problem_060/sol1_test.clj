(ns clojure-algorithms.project-euler.problem-060.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-060.sol1 :refer [prime? sum-digits concat-prime? add-prime generate-primes solution]]))

(deftest prime?-test
  (is (false? (prime? 0)))
  (is (false? (prime? 1)))
  (is (true? (prime? 2)))
  (is (true? (prime? 673)))
  (is (true? (prime? 1097)))
  (is (true? (prime? 7109))))

(deftest sum-digits-test
  (is (thrown? IllegalArgumentException (sum-digits -18)))
  (is (= 0 (sum-digits 0)))
  (is (= 5 (sum-digits 5)))
  (is (= 7 (sum-digits 79)))
  (is (= 9 (sum-digits 999))))

(deftest concat-prime?-test
  (is (true? (concat-prime? 3 7)))
  (is (false? (concat-prime? 1 6)))
  (is (true? (concat-prime? 7 109)))
  (is (false? (concat-prime? 13 31))))

(deftest add-prime-test
  (is (= [3 7 13 19 31] (add-prime [3 7 13 19])))
  (is (= [3 5 11 17 23] (add-prime [3 5 11 17])))
  (is (= [3 7 13 19 31 37] (add-prime [3 7 13 19 31]))))

(deftest generate-primes-test
  (is (= [3 7 13 19 31] (generate-primes 5 1)))
  (is (= [3 5 11 17 23] (generate-primes 5 2))))

(deftest solution-test
  (is (thrown? IllegalArgumentException (solution 3 100 nil)))
  (is (= 10 (solution 2 100 1)))
  (is (= 715 (solution 3 100 2)))
  (is (= 792 (solution 4 100 1))))
