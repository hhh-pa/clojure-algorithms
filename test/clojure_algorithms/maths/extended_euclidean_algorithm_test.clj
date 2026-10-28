(ns clojure-algorithms.maths.extended-euclidean-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.extended-euclidean-algorithm :as sut]))

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

(deftest test-fn-extended-euclidean-algorithm-examples
  (is (approx [1 0] (sut/extended-euclidean-algorithm 1 24)))
  (is (approx [2 -1] (sut/extended-euclidean-algorithm 8 14)))
  (is (approx [-9 47] (sut/extended-euclidean-algorithm 240 46)))
  (is (approx [1 0] (sut/extended-euclidean-algorithm 1 -4)))
  (is (approx [-1 0] (sut/extended-euclidean-algorithm -2 -4)))
  (is (approx [0 -1] (sut/extended-euclidean-algorithm 0 -4)))
  (is (approx [1 0] (sut/extended-euclidean-algorithm 2 0))))
