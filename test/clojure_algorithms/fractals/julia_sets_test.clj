(ns clojure-algorithms.fractals.julia-sets-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.julia-sets
             :refer [eval-exponential eval-quadratic-polynomial prepare-grid
                     iterate-function inside-escape-radius? c-abs]]))

(defn- approx= [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(defn- approx-vec= [a b] (and (= (count a) (count b)) (every? true? (map approx= a b))))

(deftest eval-test
  (is (approx-vec= [1.0 0.0] (eval-exponential [0 0] [0 0])))
  (is (< (c-abs (eval-exponential [1 0] [0 Math/PI])) 1e-9))
  (is (approx-vec= [1.0 1.0] (eval-exponential [0 1] [0 0])))
  (is (= [4 0] (eval-quadratic-polynomial [0 0] [2 0])))
  (is (= [0 0] (eval-quadratic-polynomial [-1 0] [1 0]))))

(deftest grid-test
  (is (= [[[-1.0 -1.0] [-1.0 0.0] [-1.0 1.0]]
          [[0.0 -1.0] [0.0 0.0] [0.0 1.0]]
          [[1.0 -1.0] [1.0 0.0] [1.0 1.0]]]
         (prepare-grid 1 3))))

(deftest iterate-test
  (let [grid [[[0.0 0.0] [1.0 0.0] [2.0 0.0]]]
        result (iterate-function eval-quadratic-polynomial [0 0] 3 grid)]
    (is (= [[[0.0 0.0] [1.0 0.0] [256.0 0.0]]] result)))
  (is (inside-escape-radius? [0.0 0.0] 2.0))
  (is (not (inside-escape-radius? [3.0 0.0] 2.0))))
