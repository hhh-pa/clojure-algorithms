(ns clojure-algorithms.maths.padovan-sequence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.padovan-sequence :as sut]))

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

(deftest test-fn-padovan-sequence-examples
  (is (approx 1 (sut/padovan-sequence 0)))
  (is (approx 1 (sut/padovan-sequence 1)))
  (is (approx 1 (sut/padovan-sequence 2)))
  (is (approx 2 (sut/padovan-sequence 3)))
  (is (approx 2 (sut/padovan-sequence 4)))
  (is (approx 3 (sut/padovan-sequence 5)))
  (is (approx 12 (sut/padovan-sequence 10)))
  (is (thrown? Exception (sut/padovan-sequence -1))))
