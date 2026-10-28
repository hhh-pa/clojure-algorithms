(ns clojure-algorithms.maths.average-absolute-deviation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.average-absolute-deviation :as sut]))

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

(deftest test-fn-average-absolute-deviation-examples
  (is (approx 0.0 (sut/average-absolute-deviation [0])))
  (is (approx 1.0 (sut/average-absolute-deviation [4 1 3 2])))
  (is (approx 20.0 (sut/average-absolute-deviation [2 70 6 50 20 8 4 0])))
  (is (approx 16.25 (sut/average-absolute-deviation [-20 0 30 15])))
  (is (thrown? Exception (sut/average-absolute-deviation []))))
