(ns clojure-algorithms.matrix.binary-search-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.binary-search-matrix :as sut]))

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

(deftest test-fn-mat-bin-search-examples
  (is (approx [0 0] (let [matrix [[1 4 7 11 15] [2 5 8 12 19] [3 6 9 16 22] [10 13 14 17 24] [18 21 23 26 30]] target 1] (sut/mat-bin-search target matrix))))
  (is (approx [-1 -1] (let [matrix [[1 4 7 11 15] [2 5 8 12 19] [3 6 9 16 22] [10 13 14 17 24] [18 21 23 26 30]] target 34] (sut/mat-bin-search target matrix)))))
