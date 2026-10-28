(ns clojure-algorithms.maths.series.arithmetic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.arithmetic :as sut]))

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

(deftest test-fn-is-arithmetic-series-examples
  (is (= true (boolean (sut/is-arithmetic-series [2 4 6]))))
  (is (= false (boolean (sut/is-arithmetic-series [3 6 12 24]))))
  (is (= true (boolean (sut/is-arithmetic-series [1 2 3]))))
  (is (thrown? Exception (sut/is-arithmetic-series 4)))
  (is (thrown? Exception (sut/is-arithmetic-series []))))

(deftest test-fn-arithmetic-mean-examples
  (is (approx 4.0 (sut/arithmetic-mean [2 4 6])))
  (is (approx 7.5 (sut/arithmetic-mean [3 6 9 12])))
  (is (thrown? Exception (sut/arithmetic-mean 4)))
  (is (approx 4.333333333333333 (sut/arithmetic-mean [4 8 1])))
  (is (approx 2.0 (sut/arithmetic-mean [1 2 3])))
  (is (thrown? Exception (sut/arithmetic-mean []))))
