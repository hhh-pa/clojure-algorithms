(ns clojure-algorithms.maths.special-numbers.weird-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.weird-number :as sut]))

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

(deftest test-fn-factors-examples
  (is (approx [1 2 3 4 6] (sut/factors 12)))
  (is (approx [1] (sut/factors 1)))
  (is (approx [1 2 4 5 10 20 25 50] (sut/factors 100))))

(deftest test-fn-abundant-examples
  (is (= true (boolean (sut/abundant 0))))
  (is (= false (boolean (sut/abundant 1))))
  (is (= true (boolean (sut/abundant 12))))
  (is (= false (boolean (sut/abundant 13))))
  (is (= true (boolean (sut/abundant 20)))))

(deftest test-fn-semi-perfect-examples
  (is (= true (boolean (sut/semi-perfect 0))))
  (is (= true (boolean (sut/semi-perfect 1))))
  (is (= true (boolean (sut/semi-perfect 12)))))

(deftest test-fn-weird-examples
  (is (= false (boolean (sut/weird 0))))
  (is (= false (boolean (sut/weird 77)))))
