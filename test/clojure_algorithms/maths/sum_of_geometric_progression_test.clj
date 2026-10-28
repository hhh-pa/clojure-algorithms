(ns clojure-algorithms.maths.sum-of-geometric-progression-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sum-of-geometric-progression :as sut]))

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

(deftest test-fn-sum-of-geometric-progression-examples
  (is (approx 1023.0 (sut/sum-of-geometric-progression 1 2 10)))
  (is (approx 11111.0 (sut/sum-of-geometric-progression 1 10 5)))
  (is (approx 0.0 (sut/sum-of-geometric-progression 0 2 10)))
  (is (approx 1.0 (sut/sum-of-geometric-progression 1 0 10)))
  (is (approx -0.0 (sut/sum-of-geometric-progression 1 2 0)))
  (is (approx -1023.0 (sut/sum-of-geometric-progression -1 2 10)))
  (is (approx -341.0 (sut/sum-of-geometric-progression 1 -2 10)))
  (is (approx -0.9990234375 (sut/sum-of-geometric-progression 1 2 -10))))
