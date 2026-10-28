(ns clojure-algorithms.maths.special-numbers.kaprekar-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.kaprekar-number :as sut]))

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

(deftest test-fn-is-kaprekar-examples
  (is (= true (boolean (sut/is-kaprekar 1))))
  (is (= true (boolean (sut/is-kaprekar 9))))
  (is (= true (boolean (sut/is-kaprekar 45))))
  (is (= true (boolean (sut/is-kaprekar 55))))
  (is (= false (boolean (sut/is-kaprekar 10))))
  (is (= false (boolean (sut/is-kaprekar 0))))
  (is (= false (boolean (sut/is-kaprekar -1))))
  (is (= true (boolean (sut/is-kaprekar 2223))))
  (is (thrown? Exception (sut/is-kaprekar 1.5))))
