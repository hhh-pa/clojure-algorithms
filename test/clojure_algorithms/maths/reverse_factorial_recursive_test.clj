(ns clojure-algorithms.maths.reverse-factorial-recursive-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.reverse-factorial-recursive :as sut]))

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

(deftest test-fn-reverse-factorial-recursive-examples
  (is (approx 5 (sut/reverse-factorial-recursive 120)))
  (is (approx 4 (sut/reverse-factorial-recursive 24)))
  (is (approx -1 (sut/reverse-factorial-recursive 150)))
  (is (approx 1 (sut/reverse-factorial-recursive 1)))
  (is (approx 2 (sut/reverse-factorial-recursive 2))))
