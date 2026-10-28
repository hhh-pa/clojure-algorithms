(ns clojure-algorithms.maths.numerical-analysis.nth-root-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.nth-root :as sut]))

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

(deftest test-fn-nth-root-examples
  (is (approx 0.0 (sut/nth-root 0 2 0.1)))
  (is (approx 0.0 (sut/nth-root 0.0 5)))
  (is (thrown? Exception (sut/nth-root "invalid input" 3 0.0001)))
  (is (thrown? Exception (sut/nth-root 4 0.5 0.0001)))
  (is (thrown? Exception (sut/nth-root 16 -4 0.001)))
  (is (thrown? Exception (sut/nth-root 4 2 "0.000001")))
  (is (thrown? Exception (sut/nth-root 9 2 -0.01)))
  (is (thrown? Exception (sut/nth-root -256 4 0.0001))))
