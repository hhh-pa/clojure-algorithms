(ns clojure-algorithms.maths.special-numbers.bell-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.bell-numbers :as sut]))

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

(deftest test-fn-bell-numbers-examples
  (is (thrown? Exception (sut/bell-numbers -2)))
  (is (approx [1] (sut/bell-numbers 0)))
  (is (approx [1 1] (sut/bell-numbers 1)))
  (is (approx [1 1 2 5 15 52] (sut/bell-numbers 5))))

(deftest test-fn-binomial-coefficient-examples
  (is (approx 10 (sut/-binomial-coefficient 5 2)))
  (is (approx 20 (sut/-binomial-coefficient 6 3))))
