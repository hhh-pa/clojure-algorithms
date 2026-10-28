(ns clojure-algorithms.maths.special-numbers.proth-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.proth-number :as sut]))

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

(deftest test-fn-proth-examples
  (is (approx 25 (sut/proth 6)))
  (is (thrown? Exception (sut/proth 0)))
  (is (thrown? Exception (sut/proth -1)))
  (is (thrown? Exception (sut/proth 6.0))))

(deftest test-fn-is-proth-number-examples
  (is (= false (boolean (sut/is-proth-number 1))))
  (is (= false (boolean (sut/is-proth-number 2))))
  (is (= true (boolean (sut/is-proth-number 3))))
  (is (= false (boolean (sut/is-proth-number 4))))
  (is (= true (boolean (sut/is-proth-number 5))))
  (is (= false (boolean (sut/is-proth-number 34))))
  (is (thrown? Exception (sut/is-proth-number -1)))
  (is (thrown? Exception (sut/is-proth-number 6.0))))
