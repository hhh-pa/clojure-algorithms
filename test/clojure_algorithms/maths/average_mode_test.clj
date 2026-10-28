(ns clojure-algorithms.maths.average-mode-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.average-mode :as sut]))

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

(deftest test-fn-mode-examples
  (is (approx [2] (sut/mode [2 3 4 5 3 4 2 5 2 2 4 2 2 2])))
  (is (approx [2] (sut/mode [3 4 5 3 4 2 5 2 2 4 4 2 2 2])))
  (is (approx [2 4] (sut/mode [3 4 5 3 4 2 5 2 2 4 4 4 2 2 4 2])))
  (is (approx ["y"] (sut/mode ["x" "y" "y" "z"])))
  (is (approx ["x" "y"] (sut/mode ["x" "x" "y" "y" "z"]))))
