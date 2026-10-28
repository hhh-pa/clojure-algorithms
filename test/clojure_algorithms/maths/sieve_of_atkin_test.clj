(ns clojure-algorithms.maths.sieve-of-atkin-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sieve-of-atkin :as sut]))

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

(deftest test-fn-sieve-of-atkin-examples
  (is (approx [2 3 5 7 11 13 17 19 23 29] (sut/sieve-of-atkin 30)))
  (is (approx [2 3 5 7] (sut/sieve-of-atkin 10)))
  (is (approx [2] (sut/sieve-of-atkin 2)))
  (is (approx [] (sut/sieve-of-atkin 1)))
  (is (approx [] (sut/sieve-of-atkin 0)))
  (is (thrown? Exception (sut/sieve-of-atkin -5))))
