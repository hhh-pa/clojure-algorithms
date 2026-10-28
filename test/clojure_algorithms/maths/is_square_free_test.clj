(ns clojure-algorithms.maths.is-square-free-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-square-free :as sut]))

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

(deftest test-fn-is-square-free-examples
  (is (= false (boolean (sut/is-square-free [1 1 2 3 4]))))
  (is (= true (boolean (sut/is-square-free [1 3 4 "sd" 0.0]))))
  (is (= true (boolean (sut/is-square-free [1 0.5 2 0.0]))))
  (is (= false (boolean (sut/is-square-free [1 2 2 5]))))
  (is (= true (boolean (sut/is-square-free "asd"))))
  (is (thrown? Exception (sut/is-square-free 24))))
