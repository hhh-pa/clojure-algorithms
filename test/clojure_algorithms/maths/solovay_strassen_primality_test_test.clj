(ns clojure-algorithms.maths.solovay-strassen-primality-test-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.solovay-strassen-primality-test :as sut]))

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

(deftest test-fn-jacobi-symbol-examples
  (is (approx -1 (sut/jacobi-symbol 2 13)))
  (is (approx 1 (sut/jacobi-symbol 5 19)))
  (is (approx 0 (sut/jacobi-symbol 7 14))))

(deftest test-fn-solovay-strassen-examples
  (is (= true (boolean (sut/solovay-strassen 13 5))))
  (is (= false (boolean (sut/solovay-strassen 9 10))))
  (is (= true (boolean (sut/solovay-strassen 17 15)))))
