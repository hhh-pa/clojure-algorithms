(ns clojure-algorithms.linear-algebra.src.power-iteration-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.src.power-iteration :refer [power-iteration]]))

(deftest test-power-iteration
  (let [[val vec] (power-iteration [[41 4 20] [4 26 30] [20 30 50]] [41 4 20])]
    (is (< (Math/abs (- val 79.66086378788381)) 1e-6))
    (is (< (Math/abs (- (Math/abs (nth vec 0)) 0.44472726)) 1e-6))
    (is (< (Math/abs (- (Math/abs (nth vec 1)) 0.46209842)) 1e-6))
    (is (< (Math/abs (- (Math/abs (nth vec 2)) 0.76725662)) 1e-6)))
  (let [[val _] (power-iteration [[2 0] [0 3]] [1 1])]
    (is (< (Math/abs (- val 3.0)) 1e-9))))
