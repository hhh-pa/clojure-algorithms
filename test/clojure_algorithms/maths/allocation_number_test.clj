(ns clojure-algorithms.maths.allocation-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.allocation-number :as sut]))

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

(deftest test-fn-allocation-num-examples
  (is (approx ["1-4161" "4162-8322" "8323-12483" "12484-16647"] (sut/allocation-num 16647 4)))
  (is (approx ["1-10000" "10001-20000" "20001-30000" "30001-40000" "40001-50000"] (sut/allocation-num 50000 5)))
  (is (thrown? Exception (sut/allocation-num 888 999)))
  (is (thrown? Exception (sut/allocation-num 888 -4))))
