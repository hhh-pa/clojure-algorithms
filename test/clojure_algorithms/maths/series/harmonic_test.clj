(ns clojure-algorithms.maths.series.harmonic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.series.harmonic :as sut]))

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

(deftest test-fn-is-harmonic-series-examples
  (is (= true (boolean (sut/is-harmonic-series [1 0.6666666666666666 0.5 0.4 0.3333333333333333]))))
  (is (= false (boolean (sut/is-harmonic-series [1 0.6666666666666666 0.4 0.3333333333333333]))))
  (is (= false (boolean (sut/is-harmonic-series [1 2 3]))))
  (is (= true (boolean (sut/is-harmonic-series [0.5 0.3333333333333333 0.25]))))
  (is (= true (boolean (sut/is-harmonic-series [0.4 0.2 0.13333333333333333 0.1 0.08]))))
  (is (thrown? Exception (sut/is-harmonic-series 4)))
  (is (thrown? Exception (sut/is-harmonic-series [])))
  (is (thrown? Exception (sut/is-harmonic-series [0])))
  (is (thrown? Exception (sut/is-harmonic-series [1 2 0 6]))))

(deftest test-fn-harmonic-mean-examples
  (is (approx 2.0 (sut/harmonic-mean [1 4 4])))
  (is (approx 5.759999999999999 (sut/harmonic-mean [3 6 9 12])))
  (is (thrown? Exception (sut/harmonic-mean 4)))
  (is (approx 1.6363636363636365 (sut/harmonic-mean [1 2 3])))
  (is (thrown? Exception (sut/harmonic-mean []))))
