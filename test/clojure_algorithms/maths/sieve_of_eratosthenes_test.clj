(ns clojure-algorithms.maths.sieve-of-eratosthenes-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sieve-of-eratosthenes :as sut]))

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

(deftest test-fn-prime-sieve-examples
  (is (approx [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47] (sut/prime-sieve 50)))
  (is (approx [2 3 5 7 11 13 17 19 23] (sut/prime-sieve 25)))
  (is (approx [2 3 5 7] (sut/prime-sieve 10)))
  (is (approx [2 3 5 7] (sut/prime-sieve 9)))
  (is (approx [2] (sut/prime-sieve 2)))
  (is (approx [] (sut/prime-sieve 1))))
