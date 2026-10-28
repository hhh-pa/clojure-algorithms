(ns clojure-algorithms.maths.next-prime-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.next-prime-number :as sut]))

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

(deftest test-fn-is-prime-examples
  (is (= true (boolean (sut/is-prime 2))))
  (is (= false (boolean (sut/is-prime 15))))
  (is (= true (boolean (sut/is-prime 19))))
  (is (= false (boolean (sut/is-prime 1))))
  (is (= false (boolean (sut/is-prime -7)))))

(deftest test-fn-next-prime-examples
  (is (approx 3 (sut/next-prime 2)))
  (is (approx 11 (sut/next-prime 7)))
  (is (approx 17 (sut/next-prime 14)))
  (is (approx 2 (sut/next-prime 0)))
  (is (approx 2 (sut/next-prime -10))))
