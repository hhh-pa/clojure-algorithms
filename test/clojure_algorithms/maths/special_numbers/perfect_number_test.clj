(ns clojure-algorithms.maths.special-numbers.perfect-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.perfect-number :as sut]))

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

(deftest test-fn-perfect-examples
  (is (= false (boolean (sut/perfect 27))))
  (is (= true (boolean (sut/perfect 28))))
  (is (= false (boolean (sut/perfect 29))))
  (is (= true (boolean (sut/perfect 6))))
  (is (= false (boolean (sut/perfect 12))))
  (is (= true (boolean (sut/perfect 496))))
  (is (= true (boolean (sut/perfect 8128))))
  (is (= false (boolean (sut/perfect 0))))
  (is (= false (boolean (sut/perfect -1))))
  (is (thrown? Exception (sut/perfect 12.34)))
  (is (thrown? Exception (sut/perfect "Hello"))))
