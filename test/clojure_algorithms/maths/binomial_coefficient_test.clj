(ns clojure-algorithms.maths.binomial-coefficient-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.binomial-coefficient :as sut]))

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

(deftest test-fn-binomial-coefficient-examples
  (is (approx 252 (sut/binomial-coefficient 10 5)))
  (is (approx 1 (sut/binomial-coefficient 10 0)))
  (is (approx 1 (sut/binomial-coefficient 0 10)))
  (is (approx 1 (sut/binomial-coefficient 10 10)))
  (is (approx 10 (sut/binomial-coefficient 5 2)))
  (is (approx 0 (sut/binomial-coefficient 5 6)))
  (is (approx 0 (sut/binomial-coefficient 3 5)))
  (is (thrown? Exception (sut/binomial-coefficient -2 3)))
  (is (thrown? Exception (sut/binomial-coefficient 5 -1)))
  (is (thrown? Exception (sut/binomial-coefficient 10 5.1))))
