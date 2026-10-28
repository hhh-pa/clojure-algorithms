(ns clojure-algorithms.maths.weighted-average-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.weighted-average :as sut]))

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

(deftest test-fn-weighted-average-examples
  (is (approx 2.0 (sut/weighted-average [1 2 3] [1 1 1])))
  (is (approx 23.333333333333332 (sut/weighted-average [10 20 30] [1 2 3])))
  (is (approx 12.5 (sut/weighted-average [5 15] [1 3])))
  (is (approx 100.0 (sut/weighted-average [100] [0.5])))
  (is (thrown? Exception (sut/weighted-average [] [])))
  (is (thrown? Exception (sut/weighted-average [1 2] [1])))
  (is (thrown? Exception (sut/weighted-average [1 2 3] [0 0 0]))))
