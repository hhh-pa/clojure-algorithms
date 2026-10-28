(ns clojure-algorithms.maths.prime-sieve-eratosthenes-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.prime-sieve-eratosthenes :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-prime-sieve-eratosthenes-examples
  (is (approx [2 3 5 7] (sut/prime-sieve-eratosthenes 10)))
  (is (approx [2 3 5 7 11 13 17 19] (sut/prime-sieve-eratosthenes 20)))
  (is (approx [2] (sut/prime-sieve-eratosthenes 2)))
  (is (approx [] (sut/prime-sieve-eratosthenes 1)))
  (is (thrown? Exception (sut/prime-sieve-eratosthenes -1)))
  (is (= :ValueError (err-type #(sut/prime-sieve-eratosthenes -1))))
  (is (= :ValueError (err-type #(sut/prime-sieve-eratosthenes 0)))))

(deftest test-prime-sieve-extra
  (is (= 25 (count (sut/prime-sieve-eratosthenes 100))))
  (is (= 2 (first (sut/prime-sieve-eratosthenes 1000))))
  (is (= 997 (last (sut/prime-sieve-eratosthenes 1000)))))
