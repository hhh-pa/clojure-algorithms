(ns clojure-algorithms.maths.sin-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sin :as sut]))

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

(deftest test-fn-sin-examples
  (is (approx 0.0 (sut/sin 0.0)))
  (is (approx 1.0 (sut/sin 90.0)))
  (is (approx 0.0 (sut/sin 180.0)))
  (is (approx -1.0 (sut/sin 270.0)))
  (is (approx 0.0118679603 (sut/sin 0.68)))
  (is (approx 0.0343762121 (sut/sin 1.97)))
  (is (approx 0.8987940463 (sut/sin 64.0)))
  (is (approx -0.9876883406 (sut/sin 9999.0)))
  (is (approx 0.5150380749 (sut/sin -689.0)))
  (is (approx 0.9999862922 (sut/sin 89.7))))
