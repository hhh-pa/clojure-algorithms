(ns clojure-algorithms.maths.ncr-combinations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.ncr-combinations :as sut]))

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
  (is (approx 10.0 (sut/combinations 5 2)))
  (is (approx 12.375 (sut/combinations 5.5 2)))
  (is (approx 1.0 (sut/combinations 10 0)))
  (is (approx 1.0 (sut/combinations 0 0)))
  (is (thrown? Exception (sut/combinations 5 -1)))
  (is (thrown? Exception (sut/combinations 5 2.5))))
