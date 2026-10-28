(ns clojure-algorithms.maths.prime-check-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.prime-check :as sut]))

(deftest test-is-prime-values
  (testing "upstream doctest values"
    (is (true? (sut/is-prime 2)))
    (is (true? (sut/is-prime 3)))
    (is (false? (sut/is-prime 4)))
    (is (true? (sut/is-prime 17)))
    (is (false? (sut/is-prime 25)))
    (is (true? (sut/is-prime 97)))
    (is (false? (sut/is-prime 100)))))

(deftest test-fn-is-prime-examples
  (is (false? (sut/is-prime 0)))
  (is (false? (sut/is-prime 1)))
  (is (false? (sut/is-prime 27)))
  (is (false? (sut/is-prime 87)))
  (is (true? (sut/is-prime 563)))
  (is (true? (sut/is-prime 2999)))
  (is (false? (sut/is-prime 67483)))
  (is (thrown? Exception (sut/is-prime 16.1)))
  (is (thrown? Exception (sut/is-prime -4))))

(deftest test-upstream-test-cases
  (testing "primes"
    (doseq [p [2 3 5 7 11 13 17 19 23 29]]
      (is (true? (sut/is-prime p)))))
  (testing "not primes"
    (is (thrown? Exception (sut/is-prime -19)))
    (doseq [n [0 1 4 6 9 15 105]]
      (is (false? (sut/is-prime n))))))
