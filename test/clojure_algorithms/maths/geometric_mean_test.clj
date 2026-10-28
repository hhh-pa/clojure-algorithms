(ns clojure-algorithms.maths.geometric-mean-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.geometric-mean :as sut]))

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

(deftest test-fn-compute-geometric-mean-examples
  (is (approx 4.0 (sut/compute-geometric-mean 2 8)))
  (is (thrown? Exception (sut/compute-geometric-mean "a" 4)))
  (is (approx 25.0 (sut/compute-geometric-mean 5 125)))
  (is (approx 0.0 (sut/compute-geometric-mean 1 0)))
  (is (approx 5.0 (sut/compute-geometric-mean 1 5 25 5)))
  (is (thrown? Exception (sut/compute-geometric-mean 2 -2)))
  (is (approx -5.0 (sut/compute-geometric-mean -5 25 1))))
