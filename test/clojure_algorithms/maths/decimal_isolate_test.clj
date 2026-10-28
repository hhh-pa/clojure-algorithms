(ns clojure-algorithms.maths.decimal-isolate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.decimal-isolate :as sut]))

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

(deftest test-fn-decimal-isolate-examples
  (is (approx 0.53 (sut/decimal-isolate 1.53 0)))
  (is (approx 0.3 (sut/decimal-isolate 35.345 1)))
  (is (approx 0.34 (sut/decimal-isolate 35.345 2)))
  (is (approx 0.345 (sut/decimal-isolate 35.345 3)))
  (is (approx -0.789 (sut/decimal-isolate -14.789 3)))
  (is (approx 0 (sut/decimal-isolate 0 2)))
  (is (approx -0.1 (sut/decimal-isolate -14.123 1)))
  (is (approx -0.12 (sut/decimal-isolate -14.123 2)))
  (is (approx -0.123 (sut/decimal-isolate -14.123 3))))
