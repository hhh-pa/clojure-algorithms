(ns clojure-algorithms.maths.special-numbers.polygonal-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.polygonal-numbers :as sut]))

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

(deftest test-fn-polygonal-num-examples
  (is (approx 0 (sut/polygonal-num 0 3)))
  (is (approx 6 (sut/polygonal-num 3 3)))
  (is (approx 25 (sut/polygonal-num 5 4)))
  (is (approx 5 (sut/polygonal-num 2 5)))
  (is (thrown? Exception (sut/polygonal-num -1 0)))
  (is (thrown? Exception (sut/polygonal-num 0 2))))
