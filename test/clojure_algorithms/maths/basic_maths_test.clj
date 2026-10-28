(ns clojure-algorithms.maths.basic-maths-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.basic-maths :as sut]))

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

(deftest test-fn-prime-factors-examples
  (is (approx [2 2 5 5] (sut/prime-factors 100)))
  (is (thrown? Exception (sut/prime-factors 0)))
  (is (thrown? Exception (sut/prime-factors -10))))

(deftest test-fn-number-of-divisors-examples
  (is (approx 9 (sut/number-of-divisors 100)))
  (is (thrown? Exception (sut/number-of-divisors 0)))
  (is (thrown? Exception (sut/number-of-divisors -10))))

(deftest test-fn-sum-of-divisors-examples
  (is (approx 217 (sut/sum-of-divisors 100)))
  (is (thrown? Exception (sut/sum-of-divisors 0)))
  (is (thrown? Exception (sut/sum-of-divisors -10))))

(deftest test-fn-euler-phi-examples
  (is (approx 40 (sut/euler-phi 100)))
  (is (thrown? Exception (sut/euler-phi 0)))
  (is (thrown? Exception (sut/euler-phi -10))))
