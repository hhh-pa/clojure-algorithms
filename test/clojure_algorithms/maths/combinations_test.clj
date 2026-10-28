(ns clojure-algorithms.maths.combinations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.combinations :as sut]))

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

(deftest test-fn-combinations-examples
  (is (approx 252 (sut/combinations 10 5)))
  (is (approx 20 (sut/combinations 6 3)))
  (is (approx 15504 (sut/combinations 20 5)))
  (is (approx 2598960 (sut/combinations 52 5)))
  (is (approx 1 (sut/combinations 0 0)))
  (is (thrown? Exception (sut/combinations -4 -5))))
