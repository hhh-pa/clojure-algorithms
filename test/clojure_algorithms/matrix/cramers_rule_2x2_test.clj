(ns clojure-algorithms.matrix.cramers-rule-2x2-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.cramers-rule-2x2 :as sut]))

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

(deftest test-fn-cramers-rule-2x2-examples
  (is (approx [0.0 0.0] (sut/cramers-rule-2x2 [2 3 0] [5 1 0])))
  (is (approx [13.0 12.5] (sut/cramers-rule-2x2 [0 4 50] [2 0 26])))
  (is (approx [4.0 -7.0] (sut/cramers-rule-2x2 [11 2 30] [1 0 4])))
  (is (approx [2.0 -1.0] (sut/cramers-rule-2x2 [4 7 1] [1 2 0])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [1 2 3] [2 4 6])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [1 2 3] [2 4 7])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [1 2 3] [11 22])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [0 1 6] [0 0 3])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [0 0 6] [0 0 3])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [1 2 3] [1 2 3])))
  (is (thrown? Exception (sut/cramers-rule-2x2 [0 4 50] [0 3 99]))))
