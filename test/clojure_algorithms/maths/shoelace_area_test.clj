(ns clojure-algorithms.maths.shoelace-area-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.shoelace-area :as sut]))

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

(deftest test-fn-shoelace-area-examples
  (is (approx 6.0 (sut/shoelace-area [[0 0] [4 0] [0 3]])))
  (is (approx 1.0 (sut/shoelace-area [[0 0] [1 0] [1 1] [0 1]])))
  (is (approx 4.0 (sut/shoelace-area [[0 0] [2 0] [2 2] [0 2]]))))
