(ns clojure-algorithms.maths.chinese-remainder-theorem-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.chinese-remainder-theorem :as sut]))

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

(deftest test-fn-extended-euclid-examples
  (is (approx [-1 2] (sut/extended-euclid 10 6)))
  (is (approx [-2 3] (sut/extended-euclid 7 5))))

(deftest test-fn-chinese-remainder-theorem-examples
  (is (approx 31 (sut/chinese-remainder-theorem 5 1 7 3)))
  (is (approx 14 (sut/chinese-remainder-theorem 6 1 4 3))))

(deftest test-fn-invert-modulo-examples
  (is (approx 3 (sut/invert-modulo 2 5)))
  (is (approx 1 (sut/invert-modulo 8 7))))

(deftest test-fn-chinese-remainder-theorem2-examples
  (is (approx 31 (sut/chinese-remainder-theorem2 5 1 7 3)))
  (is (approx 14 (sut/chinese-remainder-theorem2 6 1 4 3))))
