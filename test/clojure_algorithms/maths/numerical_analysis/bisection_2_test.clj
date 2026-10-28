(ns clojure-algorithms.maths.numerical-analysis.bisection-2-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.bisection-2 :as sut]))

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

(deftest test-fn-equation-examples
  (is (approx -15 (sut/equation 5)))
  (is (approx 10 (sut/equation 0)))
  (is (approx -15 (sut/equation -5)))
  (is (approx 9.99 (sut/equation 0.1)))
  (is (approx 9.99 (sut/equation -0.1))))

(deftest test-fn-bisection-examples
  (is (approx 3.1611328125 (sut/bisection -2 5)))
  (is (approx 3.158203125 (sut/bisection 0 6)))
  (is (thrown? Exception (sut/bisection 2 3))))
