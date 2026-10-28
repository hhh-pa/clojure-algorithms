(ns clojure-algorithms.maths.special-numbers.disarum-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.disarum-number :as sut]))

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

(deftest test-fn-is-disarium-number-examples
  (is (= false (boolean (sut/is-disarium-number -1))))
  (is (= false (boolean (sut/is-disarium-number 0))))
  (is (= true (boolean (sut/is-disarium-number 1))))
  (is (= true (boolean (sut/is-disarium-number 89))))
  (is (= true (boolean (sut/is-disarium-number 135))))
  (is (= true (boolean (sut/is-disarium-number 175))))
  (is (= true (boolean (sut/is-disarium-number 518))))
  (is (= true (boolean (sut/is-disarium-number 598))))
  (is (= false (boolean (sut/is-disarium-number 123))))
  (is (thrown? Exception (sut/is-disarium-number 89.0))))
