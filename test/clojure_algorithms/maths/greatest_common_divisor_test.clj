(ns clojure-algorithms.maths.greatest-common-divisor-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.greatest-common-divisor :as sut]))

(deftest test-fn-greatest-common-divisor-examples
  (is (approx 8 (sut/greatest-common-divisor 24 40)))
  (is (approx 1 (sut/greatest-common-divisor 1 1)))
  (is (approx 1 (sut/greatest-common-divisor 1 800)))
  (is (approx 1 (sut/greatest-common-divisor 11 37)))
  (is (approx 1 (sut/greatest-common-divisor 3 5)))
  (is (approx 4 (sut/greatest-common-divisor 16 4)))
  (is (approx 3 (sut/greatest-common-divisor -3 9)))
  (is (approx 3 (sut/greatest-common-divisor 9 -3)))
  (is (approx 3 (sut/greatest-common-divisor 3 -9)))
  (is (approx 3 (sut/greatest-common-divisor -3 -9)))
  (is (approx 0 (sut/greatest-common-divisor 0 0))))

(deftest test-fn-gcd-by-iterative-examples
  (is (approx 8 (sut/gcd-by-iterative 24 40)))
  (is (approx (sut/greatest-common-divisor 24 40) (sut/gcd-by-iterative 24 40)))
  (is (approx 3 (sut/gcd-by-iterative -3 -9)))
  (is (approx 3 (sut/gcd-by-iterative 3 -9)))
  (is (approx 1 (sut/gcd-by-iterative 1 -800)))
  (is (approx 1 (sut/gcd-by-iterative 11 37)))
  (is (approx 0 (sut/gcd-by-iterative 0 0))))
