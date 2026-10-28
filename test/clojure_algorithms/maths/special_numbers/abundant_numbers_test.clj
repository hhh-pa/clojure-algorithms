(ns clojure-algorithms.maths.special-numbers.abundant-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.abundant-numbers :as sut]))

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

(deftest test-fn-is-abundant-number-examples
  (is (= false (boolean (sut/is-abundant-number -1))))
  (is (= false (boolean (sut/is-abundant-number 0))))
  (is (= true (boolean (sut/is-abundant-number 12))))
  (is (= true (boolean (sut/is-abundant-number 18))))
  (is (= false (boolean (sut/is-abundant-number 28))))
  (is (= true (boolean (sut/is-abundant-number 20))))
  (is (= false (boolean (sut/is-abundant-number 6))))
  (is (= false (boolean (sut/is-abundant-number 1))))
  (is (= true (boolean (sut/is-abundant-number 945))))
  (is (thrown? Exception (sut/is-abundant-number 28.0))))
