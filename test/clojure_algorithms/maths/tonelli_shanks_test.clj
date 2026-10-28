(ns clojure-algorithms.maths.tonelli-shanks-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.tonelli-shanks :as sut]))

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

(deftest test-fn-legendre-symbol-examples
  (is (approx 1 (sut/legendre-symbol 2 7)))
  (is (approx -1 (sut/legendre-symbol 3 7)))
  (is (approx 0 (sut/legendre-symbol 14 7)))
  (is (approx 1 (sut/legendre-symbol 5 11))))

(deftest test-fn-tonelli-shanks-examples
  (is (approx 13 (sut/tonelli-shanks 5 41)))
  (is (approx 3 (sut/tonelli-shanks 2 7)))
  (is (approx 6 (sut/tonelli-shanks 10 13)))
  (is (approx 0 (sut/tonelli-shanks 0 11)))
  (is (approx 5 (sut/tonelli-shanks 8 17)))
  (is (thrown? Exception (sut/tonelli-shanks 3 7)))
  (is (thrown? Exception (sut/tonelli-shanks 5 4)))
  (is (thrown? Exception (sut/tonelli-shanks 5 1))))
