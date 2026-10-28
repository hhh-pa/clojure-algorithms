(ns clojure-algorithms.matrix.kronecker-product-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.kronecker-product :as sut]))

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

(deftest test-fn-is-2d-examples
  (is (= true (boolean (sut/is-2d []))))
  (is (= false (boolean (sut/is-2d [1 2]))))
  (is (= true (boolean (sut/is-2d [[1 2] [3 4]])))))

(deftest test-fn-kronecker-product-examples
  (is (approx [[5 6 10 12] [7 8 14 16]] (sut/kronecker-product [[1 2]] [[5 6] [7 8]])))
  (is (approx [[5 6 10 12] [7 8 14 16] [20 24 25 30] [28 32 35 40]] (sut/kronecker-product [[1 2] [4 5]] [[5 6] [7 8]])))
  (is (thrown? Exception (sut/kronecker-product [1 2] [[5 6] [7 8]]))))
