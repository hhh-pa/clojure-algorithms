(ns clojure-algorithms.maths.germain-primes-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.germain-primes :as sut]))

(deftest test-fn-is-germain-prime-examples
  (is (= true (boolean (sut/is-germain-prime 3))))
  (is (= true (boolean (sut/is-germain-prime 11))))
  (is (= false (boolean (sut/is-germain-prime 4))))
  (is (= true (boolean (sut/is-germain-prime 23))))
  (is (= false (boolean (sut/is-germain-prime 13))))
  (is (= false (boolean (sut/is-germain-prime 20))))
  (is (thrown? Exception (sut/is-germain-prime "abc"))))

(deftest test-fn-is-safe-prime-examples
  (is (= true (boolean (sut/is-safe-prime 5))))
  (is (= true (boolean (sut/is-safe-prime 11))))
  (is (= false (boolean (sut/is-safe-prime 1))))
  (is (= false (boolean (sut/is-safe-prime 2))))
  (is (= false (boolean (sut/is-safe-prime 3))))
  (is (= true (boolean (sut/is-safe-prime 47))))
  (is (thrown? Exception (sut/is-safe-prime "abc"))))
