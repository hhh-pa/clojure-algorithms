(ns clojure-algorithms.maths.special-numbers.hamming-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.hamming-numbers :as sut]))

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

(deftest test-fn-hamming-examples
  (is (thrown? Exception (sut/hamming -5)))
  (is (approx [1 2 3 4 5] (sut/hamming 5)))
  (is (approx [1 2 3 4 5 6 8 9 10 12] (sut/hamming 10)))
  (is (approx [1 2 3 4 5 6 8 9 10 12 15 16 18 20 24] (sut/hamming 15))))
