(ns clojure-algorithms.maths.persistence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.persistence :as sut]))

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

(deftest test-fn-multiplicative-persistence-examples
  (is (approx 2 (sut/multiplicative-persistence 217)))
  (is (thrown? Exception (sut/multiplicative-persistence -1)))
  (is (thrown? Exception (sut/multiplicative-persistence "long number"))))

(deftest test-fn-additive-persistence-examples
  (is (approx 3 (sut/additive-persistence 199)))
  (is (thrown? Exception (sut/additive-persistence -1)))
  (is (thrown? Exception (sut/additive-persistence "long number"))))
