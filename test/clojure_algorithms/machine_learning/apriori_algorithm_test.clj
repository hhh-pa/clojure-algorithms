(ns clojure-algorithms.machine-learning.apriori-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.apriori-algorithm :as sut]))

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

(deftest test-fn-load-data-examples
  (is (approx [["milk"] ["milk" "butter"] ["milk" "bread"] ["milk" "bread" "chips"]] (sut/load-data))))

(deftest test-fn-prune-examples
  (is (approx [["X" "Y"] ["X" "Z"] ["Y" "Z"]] (let [itemset ["X" "Y" "Z"] candidates [["X" "Y"] ["X" "Z"] ["Y" "Z"]]] (sut/prune itemset candidates 2)))))

(deftest test-fn-apriori-examples
  (is (approx [[["A" "B"] 1] [["A" "C"] 2] [["B" "C"] 2]] (let [data [["A" "B" "C"] ["A" "B"] ["A" "C"] ["A" "D"] ["B" "C"]]] (sut/apriori data 2))))
  (is (approx [] (let [data [["1" "2" "3"] ["1" "2"] ["1" "3"] ["1" "4"] ["2" "3"]]] (sut/apriori data 3)))))
