(ns clojure-algorithms.maths.modular-division-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.modular-division :as sut]))

(deftest test-fn-modular-division-examples
  (is (approx 2 (sut/modular-division 4 8 5)))
  (is (approx 1 (sut/modular-division 3 8 5)))
  (is (approx 4 (sut/modular-division 4 11 5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/modular-division 4 8 1)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/modular-division 0 8 5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/modular-division 4 8 6))))

(deftest test-fn-invert-modulo-examples
  (is (approx 3 (sut/invert-modulo 2 5)))
  (is (approx 1 (sut/invert-modulo 8 7))))

(deftest test-fn-modular-division2-examples
  (is (approx 2 (sut/modular-division2 4 8 5)))
  (is (approx 1 (sut/modular-division2 3 8 5)))
  (is (approx 4 (sut/modular-division2 4 11 5))))

(deftest test-fn-extended-gcd-examples
  (is (approx [2 -1 2] (sut/extended-gcd 10 6)))
  (is (approx [1 -2 3] (sut/extended-gcd 7 5))))

(deftest test-fn-extended-euclid-examples
  (is (approx [-1 2] (sut/extended-euclid 10 6)))
  (is (approx [-2 3] (sut/extended-euclid 7 5))))

(deftest test-fn-greatest-common-divisor-examples
  (is (approx 1 (sut/greatest-common-divisor 7 5)))
  (is (approx 11 (sut/greatest-common-divisor 121 11))))
