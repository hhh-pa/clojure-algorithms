(ns clojure-algorithms.maths.special-numbers.krishnamurthy-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.krishnamurthy-number :as sut]))

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

(deftest test-fn-factorial-examples
  (is (approx 6 (sut/factorial 3)))
  (is (approx 1 (sut/factorial 0)))
  (is (approx 120 (sut/factorial 5))))

(deftest test-fn-krishnamurthy-examples
  (is (= true (boolean (sut/krishnamurthy 145))))
  (is (= false (boolean (sut/krishnamurthy 240))))
  (is (= true (boolean (sut/krishnamurthy 1)))))
