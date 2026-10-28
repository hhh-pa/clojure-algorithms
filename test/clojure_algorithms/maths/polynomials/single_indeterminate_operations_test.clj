(ns clojure-algorithms.maths.polynomials.single-indeterminate-operations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.polynomials.single-indeterminate-operations :as sut]))

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

(deftest test-fn-repr-examples
  (is (some? (let [p (sut/make-Polynomial 2 [1 2 3])] p))))

(deftest test-fn-eq-examples
  (is (let [p (sut/make-Polynomial 2 [1 2 3]) q (sut/make-Polynomial 2 [1 2 3])] (approx p q))))

(deftest test-fn-ne-examples
  (is (let [p (sut/make-Polynomial 2 [1 2 3]) q (sut/make-Polynomial 2 [1 2 3])] (approx p q))))
