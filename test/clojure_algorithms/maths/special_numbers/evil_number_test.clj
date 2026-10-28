(ns clojure-algorithms.maths.special-numbers.evil-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.evil-number :as sut]))

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

(deftest test-fn-is-evil-number-examples
  (is (= true (boolean (sut/is-evil-number 0))))
  (is (= true (boolean (sut/is-evil-number 3))))
  (is (= true (boolean (sut/is-evil-number 5))))
  (is (= true (boolean (sut/is-evil-number 6))))
  (is (= true (boolean (sut/is-evil-number 9))))
  (is (= false (boolean (sut/is-evil-number 1))))
  (is (= false (boolean (sut/is-evil-number 2))))
  (is (= false (boolean (sut/is-evil-number 4))))
  (is (= false (boolean (sut/is-evil-number 7))))
  (is (= false (boolean (sut/is-evil-number 8))))
  (is (thrown? Exception (sut/is-evil-number -1)))
  (is (thrown? Exception (sut/is-evil-number 5.5))))
