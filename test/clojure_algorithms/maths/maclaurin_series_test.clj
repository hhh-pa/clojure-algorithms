(ns clojure-algorithms.maths.maclaurin-series-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.maclaurin-series :as sut]))

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

(deftest test-fn-maclaurin-sin-examples
  (is (approx -0.544021110889369 (sut/maclaurin-sin 10)))
  (is (approx 0.5440211108893703 (sut/maclaurin-sin -10)))
  (is (approx -0.5440211108893689 (sut/maclaurin-sin 10 15)))
  (is (approx 0.5440211108893703 (sut/maclaurin-sin -10 15)))
  (is (thrown? Exception (sut/maclaurin-sin "10")))
  (is (thrown? Exception (sut/maclaurin-sin 10 -30)))
  (is (thrown? Exception (sut/maclaurin-sin 10 30.5)))
  (is (thrown? Exception (sut/maclaurin-sin 10 "30"))))

(deftest test-fn-maclaurin-cos-examples
  (is (approx 0.28366218546322675 (sut/maclaurin-cos 5)))
  (is (approx 0.2836621854632266 (sut/maclaurin-cos -5)))
  (is (approx -0.8390715290764525 (sut/maclaurin-cos 10 15)))
  (is (approx -0.8390715290764521 (sut/maclaurin-cos -10 15)))
  (is (thrown? Exception (sut/maclaurin-cos "10")))
  (is (thrown? Exception (sut/maclaurin-cos 10 -30)))
  (is (thrown? Exception (sut/maclaurin-cos 10 30.5)))
  (is (thrown? Exception (sut/maclaurin-cos 10 "30"))))
