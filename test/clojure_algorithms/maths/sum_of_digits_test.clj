(ns clojure-algorithms.maths.sum-of-digits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sum-of-digits :as sut]))

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

(deftest test-fn-sum-of-digits-examples
  (is (approx 15 (sut/sum-of-digits 12345)))
  (is (approx 6 (sut/sum-of-digits 123)))
  (is (approx 6 (sut/sum-of-digits -123)))
  (is (approx 0 (sut/sum-of-digits 0))))

(deftest test-fn-sum-of-digits-recursion-examples
  (is (approx 15 (sut/sum-of-digits-recursion 12345)))
  (is (approx 6 (sut/sum-of-digits-recursion 123)))
  (is (approx 6 (sut/sum-of-digits-recursion -123)))
  (is (approx 0 (sut/sum-of-digits-recursion 0))))

(deftest test-fn-sum-of-digits-compact-examples
  (is (approx 15 (sut/sum-of-digits-compact 12345)))
  (is (approx 6 (sut/sum-of-digits-compact 123)))
  (is (approx 6 (sut/sum-of-digits-compact -123)))
  (is (approx 0 (sut/sum-of-digits-compact 0))))
