(ns clojure-algorithms.maths.factorial-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.factorial :as sut]))

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

(deftest test-fn-factorial-examples
  (is (approx 1 (sut/factorial 0)))
  (is (approx 1 (sut/factorial 1)))
  (is (approx 120 (sut/factorial 5)))
  (is (thrown? Exception (sut/factorial 0.1)))
  (is (thrown? Exception (sut/factorial -1))))

(deftest test-fn-factorial-recursive-examples
  (is (approx 1 (sut/factorial-recursive 0)))
  (is (approx 1 (sut/factorial-recursive 1)))
  (is (approx 120 (sut/factorial-recursive 5)))
  (is (thrown? Exception (sut/factorial-recursive 0.1)))
  (is (thrown? Exception (sut/factorial-recursive -1))))
