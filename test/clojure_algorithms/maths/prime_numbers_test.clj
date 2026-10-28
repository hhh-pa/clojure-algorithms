(ns clojure-algorithms.maths.prime-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.prime-numbers :as sut]))

(deftest test-fn-slow-primes-examples
  (is (approx [] (vec (sut/slow-primes 0))))
  (is (approx [] (vec (sut/slow-primes -1))))
  (is (approx [] (vec (sut/slow-primes -10))))
  (is (approx [2 3 5 7 11 13 17 19 23] (vec (sut/slow-primes 25))))
  (is (approx [2 3 5 7 11] (vec (sut/slow-primes 11))))
  (is (approx [2 3 5 7 11 13 17 19 23 29 31] (vec (sut/slow-primes 33))))
  (is (approx 997 (last (sut/slow-primes 1000)))))

(deftest test-fn-primes-examples
  (is (approx [] (vec (sut/primes 0))))
  (is (approx [] (vec (sut/primes -1))))
  (is (approx [] (vec (sut/primes -10))))
  (is (approx [2 3 5 7 11 13 17 19 23] (vec (sut/primes 25))))
  (is (approx [2 3 5 7 11] (vec (sut/primes 11))))
  (is (approx [2 3 5 7 11 13 17 19 23 29 31] (vec (sut/primes 33))))
  (is (approx 997 (last (sut/primes 1000)))))

(deftest test-fn-fast-primes-examples
  (is (approx [] (vec (sut/fast-primes 0))))
  (is (approx [] (vec (sut/fast-primes -1))))
  (is (approx [] (vec (sut/fast-primes -10))))
  (is (approx [2 3 5 7 11 13 17 19 23] (vec (sut/fast-primes 25))))
  (is (approx [2 3 5 7 11] (vec (sut/fast-primes 11))))
  (is (approx [2 3 5 7 11 13 17 19 23 29 31] (vec (sut/fast-primes 33))))
  (is (approx 997 (last (sut/fast-primes 1000)))))

(deftest test-fn-benchmark-stub
  (is (= :UnsupportedOperation (try (sut/benchmark) nil
                                    (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
