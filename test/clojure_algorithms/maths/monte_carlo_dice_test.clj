(ns clojure-algorithms.maths.monte-carlo-dice-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.monte-carlo-dice :as sut]))

(deftest test-dice-basics
  (is (= 6 sut/Dice-NUM-SIDES))
  (is (= [1 2 3 4 5 6] (:sides (sut/make-Dice))))
  (is (= 4 (sut/Dice-roll (sut/make-Dice) (sut/rng 0))))
  (is (every? #{1 2 3 4 5 6}
              (let [r (sut/rng 7)
                    d (sut/make-Dice)]
                (repeatedly 50 #(sut/Dice-roll d r))))))

(deftest test-fn-throw-dice-examples
  (let [r (sut/rng 0)]
    (is (approx [10.0 0.0 30.0 50.0 10.0 0.0] (sut/throw-dice 10 1 r)))
    (is (approx [19.0 17.0 17.0 11.0 23.0 13.0] (sut/throw-dice 100 1 r)))
    (is (approx [18.8 15.5 16.3 17.6 14.2 17.6] (sut/throw-dice 1000 1 r)))
    (is (approx [16.35 16.89 16.93 16.6 16.52 16.71] (sut/throw-dice 10000 1 r)))
    (is (approx [2.74 5.6 7.99 11.26 13.92 16.7 14.44 10.63 8.05 5.92 2.75]
                (sut/throw-dice 10000 2 r)))))

(deftest test-fn-throw-dice-seeded
  (is (approx [10.0 0.0 30.0 50.0 10.0 0.0] (sut/throw-dice 10 1 0)))
  (is (= (sut/throw-dice 100 2 42) (sut/throw-dice 100 2 42)))
  (is (= (sut/throw-dice 100 2 (sut/rng 42)) (sut/throw-dice 100 2 42))))

(deftest test-fn-throw-dice-statistical
  (let [probs (sut/throw-dice 1000)]
    (is (= 11 (count probs)))
    (is (< (Math/abs (- (reduce + probs) 100.0)) 0.5))
    (is (every? #(<= 0.0 % 100.0) probs)))
  (let [probs (sut/throw-dice 1000 1)]
    (is (= 6 (count probs)))
    (is (< (Math/abs (- (reduce + probs) 100.0)) 0.5))))
