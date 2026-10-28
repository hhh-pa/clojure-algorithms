(ns clojure-algorithms.maths.series.p-series-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.p-series :as sut]))

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

(deftest test-fn-p-series-examples
  (is (approx ["1" "1 / 4" "1 / 9" "1 / 16" "1 / 25"] (sut/p-series 5 2)))
  (is (approx [] (sut/p-series -5 2)))
  (is (approx ["1" "1 / 0.25" "1 / 0.1111111111111111" "1 / 0.0625" "1 / 0.04"] (sut/p-series 5 -2)))
  (is (approx [""] (sut/p-series "" 1000)))
  (is (approx [] (sut/p-series 0 0)))
  (is (approx ["1"] (sut/p-series 1 1))))
