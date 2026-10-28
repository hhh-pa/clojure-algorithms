(ns clojure-algorithms.maths.max-sum-sliding-window-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.max-sum-sliding-window :as sut]))

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

(deftest test-fn-max-sum-in-array-examples
  (is (approx 24 (let [arr [1 4 2 10 2 3 1 0 20] k 4] (sut/max-sum-in-array arr k))))
  (is (thrown? Exception (let [arr [1 4 2 10 2 3 1 0 20] k 10] (sut/max-sum-in-array arr k))))
  (is (approx 27 (let [arr [1 4 2 10 2 13 1 0 2] k 4] (sut/max-sum-in-array arr k)))))
