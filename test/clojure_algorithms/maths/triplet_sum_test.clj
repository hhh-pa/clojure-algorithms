(ns clojure-algorithms.maths.triplet-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.triplet-sum :as sut]))

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

(deftest test-fn-triplet-sum1-examples
  (is (approx [5 7 23] (sut/triplet-sum1 [13 29 7 23 5] 35)))
  (is (approx [9 19 37] (sut/triplet-sum1 [37 9 19 50 44] 65)))
  (is (approx [0 0 0] (let [arr [6 47 27 1 15] target 11] (sut/triplet-sum1 arr target)))))

(deftest test-fn-triplet-sum2-examples
  (is (approx [5 7 23] (sut/triplet-sum2 [13 29 7 23 5] 35)))
  (is (approx [9 19 37] (sut/triplet-sum2 [37 9 19 50 44] 65)))
  (is (approx [0 0 0] (let [arr [6 47 27 1 15] target 11] (sut/triplet-sum2 arr target)))))
