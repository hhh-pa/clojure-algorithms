(ns clojure-algorithms.fractals.hilbert-curve-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.hilbert-curve
             :refer [rotate-pnts hilbert-curve]]))

(deftest rotate-test
  (is (= [[0 1] [0 0] [1 0] [1 1]]
         (rotate-pnts [[1 1] [0 1] [0 0] [1 0]] 90))))

(deftest hilbert-test
  (is (= [[0.25 0.25] [-0.25 0.25] [-0.25 -0.25] [0.25 -0.25]]
         (hilbert-curve [0 0] 1 1 0)))
  (is (= 4 (count (hilbert-curve [0 0] 1))))
  (is (= 16 (count (hilbert-curve [0 0] 2))))
  (is (= 64 (count (hilbert-curve [0 0] 3)))))
