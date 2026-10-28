(ns clojure-algorithms.maths.check-polygon-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.check-polygon :as sut]))

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

(deftest test-fn-check-polygon-examples
  (is (= true (boolean (sut/check-polygon [6 10 5]))))
  (is (= false (boolean (sut/check-polygon [3 7 13 2]))))
  (is (= false (boolean (sut/check-polygon [1 4.3 5.2 12.2]))))
  (is (approx [3 7 13 2] (let [nums [3 7 13 2] --v (sut/check-polygon nums)] nums)))
  (is (thrown? Exception (sut/check-polygon [])))
  (is (thrown? Exception (sut/check-polygon [4 5])))
  (is (thrown? Exception (sut/check-polygon [-2 5 6]))))
