(ns clojure-algorithms.maths.two-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.two-sum :as sut]))

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

(deftest test-fn-two-sum-examples
  (is (approx [0 1] (sut/two-sum [2 7 11 15] 9)))
  (is (approx [1 2] (sut/two-sum [15 2 11 7] 13)))
  (is (approx [0 3] (sut/two-sum [2 7 11 15] 17)))
  (is (approx [0 2] (sut/two-sum [7 15 11 2] 18)))
  (is (approx [2 3] (sut/two-sum [2 7 11 15] 26)))
  (is (approx [] (sut/two-sum [2 7 11 15] 8))))
