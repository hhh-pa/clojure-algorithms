(ns clojure-algorithms.maths.signum-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.signum :as sut]))

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

(deftest test-fn-signum-examples
  (is (approx -1 (sut/signum -10)))
  (is (approx 1 (sut/signum 10)))
  (is (approx 0 (sut/signum 0)))
  (is (approx -1 (sut/signum -20.5)))
  (is (approx 1 (sut/signum 20.5)))
  (is (thrown? Exception (sut/signum "Hello")))
  (is (thrown? Exception (sut/signum []))))

(deftest test-fn-test-signum-examples
  (is (nil? (sut/test-signum))))
