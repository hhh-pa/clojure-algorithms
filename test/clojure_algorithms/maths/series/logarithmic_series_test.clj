(ns clojure-algorithms.maths.series.logarithmic-series-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.logarithmic-series :as sut]))

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

(deftest test-fn-logarithmic-series-examples
  (is (approx ["(2^1)/1" "-(2^2)/2" "(2^3)/3" "-(2^4)/4" "(2^5)/5"] (sut/logarithmic-series 3)))
  (is (approx ["-(4^1)/1" "(4^2)/2" "-(4^3)/3" "(4^4)/4" "-(4^5)/5"] (sut/logarithmic-series -3)))
  (is (approx ["(2^1)/1" "-(2^2)/2" "(2^3)/3" "-(2^4)/4" "(2^5)/5" "-(2^6)/6"] (sut/logarithmic-series 3 6))))
