(ns clojure-algorithms.maths.power-using-recursion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.power-using-recursion :as sut]))

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

(deftest test-fn-power-examples
  (is (approx 81 (sut/power 3 4)))
  (is (approx 1 (sut/power 2 0)))
  (is (= "a" (str (sut/power "a" 1))))
  (is (thrown? Exception (sut/power "a" 2)))
  (is (thrown? Exception (sut/power "a" "b")))
  (is (thrown? Throwable (sut/power 2 -1)))
  (is (approx 1 (sut/power 0 0)))
  (is (approx 0 (sut/power 0 1)))
  (is (approx 15625 (sut/power 5 6)))
  (is (approx 21914624432020321 (sut/power 23 12))))
