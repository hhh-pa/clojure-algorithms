(ns clojure-algorithms.maths.convolve-1d-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.convolve-1d :as sut]))

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

(deftest test-class-signal-examples
  (is (some? (sut/make-Signal [1.0 3.0 2.0 -1.0]))))

(deftest test-class-discreteconvolve1d-examples
  (is (some? (let [s1 (sut/make-Signal [1 2 3 4 5]) s2 (sut/make-Signal [1 -1 2 -3])] (sut/make-DiscreteConvolve1D s1 s2)))))
