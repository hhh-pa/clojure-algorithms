(ns clojure-algorithms.maths.lucas-series-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.lucas-series :as sut]))

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

(deftest test-fn-recursive-lucas-number-examples
  (is (approx 1 (sut/recursive-lucas-number 1)))
  (is (approx 15127 (sut/recursive-lucas-number 20)))
  (is (approx 2 (sut/recursive-lucas-number 0)))
  (is (approx 167761 (sut/recursive-lucas-number 25)))
  (is (thrown? Exception (sut/recursive-lucas-number -1.5))))

(deftest test-fn-dynamic-lucas-number-examples
  (is (approx 1 (sut/dynamic-lucas-number 1)))
  (is (approx 15127 (sut/dynamic-lucas-number 20)))
  (is (approx 2 (sut/dynamic-lucas-number 0)))
  (is (approx 167761 (sut/dynamic-lucas-number 25)))
  (is (thrown? Exception (sut/dynamic-lucas-number -1.5))))
