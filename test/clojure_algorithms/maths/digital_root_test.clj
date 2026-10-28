(ns clojure-algorithms.maths.digital-root-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.digital-root :as sut]))

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

(deftest test-fn-digital-root-examples
  (is (approx 0 (sut/digital-root 0)))
  (is (approx 9 (sut/digital-root 9)))
  (is (approx 2 (sut/digital-root 38)))
  (is (approx 6 (sut/digital-root 12345)))
  (is (approx 9 (sut/digital-root -45)))
  (is (approx 9 (sut/digital-root 999999999999))))
