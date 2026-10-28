(ns clojure-algorithms.data-structures.hashing.number-theory.prime-numbers-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.number-theory.prime-numbers
             :refer [is-prime next-prime]]))

(deftest test-is-prime
  (is (false? (is-prime 0)))
  (is (false? (is-prime 1)))
  (is (true? (is-prime 2)))
  (is (true? (is-prime 3)))
  (is (false? (is-prime 27)))
  (is (false? (is-prime 87)))
  (is (true? (is-prime 563)))
  (is (true? (is-prime 2999)))
  (is (false? (is-prime 67483)))
  (is (thrown? clojure.lang.ExceptionInfo (is-prime -5))))

(deftest test-next-prime
  (is (= 7 (next-prime 5)))
  (is (= 7 (next-prime 6)))
  (is (= 5 (next-prime 4)))
  (is (= 7 (next-prime 3 2)))
  (is (= 11 (next-prime 5 2)))
  (is (= 23 (next-prime 10 2)))
  (is (= 17 (next-prime 7 2))))
