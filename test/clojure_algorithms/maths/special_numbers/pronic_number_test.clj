(ns clojure-algorithms.maths.special-numbers.pronic-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.pronic-number :as sut]))

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

(deftest test-fn-is-pronic-examples
  (is (= false (boolean (sut/is-pronic -1))))
  (is (= true (boolean (sut/is-pronic 0))))
  (is (= true (boolean (sut/is-pronic 2))))
  (is (= false (boolean (sut/is-pronic 5))))
  (is (= true (boolean (sut/is-pronic 6))))
  (is (= false (boolean (sut/is-pronic 8))))
  (is (= true (boolean (sut/is-pronic 30))))
  (is (= false (boolean (sut/is-pronic 32))))
  (is (= true (boolean (sut/is-pronic 2147441940))))
  (is (= true (boolean (sut/is-pronic 9223372033963249500))))
  (is (thrown? Exception (sut/is-pronic 6.0))))
