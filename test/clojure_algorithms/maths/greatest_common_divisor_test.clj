(ns clojure-algorithms.maths.greatest-common-divisor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.greatest-common-divisor :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

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
