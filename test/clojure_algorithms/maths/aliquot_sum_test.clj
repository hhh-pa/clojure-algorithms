(ns clojure-algorithms.maths.aliquot-sum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.aliquot-sum :as sut]))

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

(deftest test-fn-aliquot-sum-examples
  (is (approx 9 (sut/aliquot-sum 15)))
  (is (approx 6 (sut/aliquot-sum 6)))
  (is (thrown? Exception (sut/aliquot-sum -1)))
  (is (thrown? Exception (sut/aliquot-sum 0)))
  (is (thrown? Exception (sut/aliquot-sum 1.6)))
  (is (approx 16 (sut/aliquot-sum 12)))
  (is (approx 0 (sut/aliquot-sum 1)))
  (is (approx 1 (sut/aliquot-sum 19))))
