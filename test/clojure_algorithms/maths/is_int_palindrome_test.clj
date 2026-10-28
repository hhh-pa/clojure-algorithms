(ns clojure-algorithms.maths.is-int-palindrome-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-int-palindrome :as sut]))

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

(deftest test-fn-is-int-palindrome-examples
  (is (= false (boolean (sut/is-int-palindrome -121))))
  (is (= true (boolean (sut/is-int-palindrome 0))))
  (is (= false (boolean (sut/is-int-palindrome 10))))
  (is (= true (boolean (sut/is-int-palindrome 11))))
  (is (= true (boolean (sut/is-int-palindrome 101))))
  (is (= false (boolean (sut/is-int-palindrome 120)))))
