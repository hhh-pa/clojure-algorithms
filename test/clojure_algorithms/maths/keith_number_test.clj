(ns clojure-algorithms.maths.keith-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.keith-number :as sut]))

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

(deftest test-fn-is-keith-number-examples
  (is (thrown? Exception (sut/is-keith-number 14)))
  (is (thrown? Exception (sut/is-keith-number 19)))
  (is (thrown? Exception (sut/is-keith-number 28)))
  (is (thrown? Exception (sut/is-keith-number 47)))
  (is (thrown? Exception (sut/is-keith-number 61)))
  (is (thrown? Exception (sut/is-keith-number 75)))
  (is (thrown? Exception (sut/is-keith-number 197)))
  (is (thrown? Exception (sut/is-keith-number 742)))
  (is (thrown? Exception (sut/is-keith-number 1104)))
  (is (thrown? Exception (sut/is-keith-number 1537)))
  (is (thrown? Exception (sut/is-keith-number 10)))
  (is (thrown? Exception (sut/is-keith-number 12)))
  (is (thrown? Exception (sut/is-keith-number 25)))
  (is (thrown? Exception (sut/is-keith-number 100)))
  (is (thrown? Exception (sut/is-keith-number 9)))
  (is (thrown? Exception (sut/is-keith-number 1)))
  (is (thrown? Exception (sut/is-keith-number 0)))
  (is (thrown? Exception (sut/is-keith-number -14)))
  (is (thrown? Exception (sut/is-keith-number 14.0)))
  (is (thrown? Exception (sut/is-keith-number "14")))
  (is (thrown? Exception (sut/is-keith-number true)))
  (is (thrown? Exception (sut/is-keith-number nil))))

(deftest test-fn-find-keith-numbers-examples
  (is (thrown? Exception (sut/find-keith-numbers 9)))
  (is (thrown? Exception (sut/find-keith-numbers 10.5)))
  (is (thrown? Exception (sut/find-keith-numbers false))))
