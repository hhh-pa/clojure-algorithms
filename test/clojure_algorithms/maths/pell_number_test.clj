(ns clojure-algorithms.maths.pell-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.pell-number :as sut]))

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

(deftest test-fn-pell-number-iterative-examples
  (is (approx 0 (sut/pell-number-iterative 0)))
  (is (approx 1 (sut/pell-number-iterative 1)))
  (is (approx 13860 (sut/pell-number-iterative 12)))
  (is (thrown? Exception (sut/pell-number-iterative "1")))
  (is (thrown? Exception (sut/pell-number-iterative -1))))

(deftest test-fn-pell-number-recursive-examples
  (is (approx 0 (sut/pell-number-recursive 0)))
  (is (approx 1 (sut/pell-number-recursive 1)))
  (is (approx 13860 (sut/pell-number-recursive 12)))
  (is (thrown? Exception (sut/pell-number-recursive "1")))
  (is (thrown? Exception (sut/pell-number-recursive -1))))
