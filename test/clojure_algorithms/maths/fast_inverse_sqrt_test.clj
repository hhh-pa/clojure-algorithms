(ns clojure-algorithms.maths.fast-inverse-sqrt-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.fast-inverse-sqrt :as sut]))

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

(deftest test-fn-fast-inverse-sqrt-examples
  (is (approx 0.3156857923527257 (sut/fast-inverse-sqrt 10)))
  (is (approx 0.49915357479239103 (sut/fast-inverse-sqrt 4)))
  (is (approx 0.4932849504615651 (sut/fast-inverse-sqrt 4.1)))
  (is (thrown? Exception (sut/fast-inverse-sqrt 0)))
  (is (thrown? Exception (sut/fast-inverse-sqrt -1))))
