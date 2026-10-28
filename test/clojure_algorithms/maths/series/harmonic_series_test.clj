(ns clojure-algorithms.maths.series.harmonic-series-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.harmonic-series :as sut]))

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

(deftest test-fn-harmonic-series-examples
  (is (approx ["1" "1/2" "1/3" "1/4" "1/5"] (sut/harmonic-series 5)))
  (is (approx ["1" "1/2" "1/3" "1/4" "1/5"] (sut/harmonic-series 5.0)))
  (is (approx ["1" "1/2" "1/3" "1/4" "1/5"] (sut/harmonic-series 5.1)))
  (is (approx [] (sut/harmonic-series -5)))
  (is (approx [] (sut/harmonic-series 0)))
  (is (approx ["1"] (sut/harmonic-series 1))))
