(ns clojure-algorithms.maths.integer-square-root-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.integer-square-root :as sut]))

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

(deftest test-fn-integer-square-root-examples
  (is (approx 25 (sut/integer-square-root 625)))
  (is (approx 46340 (sut/integer-square-root 2147483647)))
  (is (thrown? Exception (sut/integer-square-root -1)))
  (is (thrown? Exception (sut/integer-square-root 1.5)))
  (is (thrown? Exception (sut/integer-square-root "0"))))
