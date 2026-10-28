(ns clojure-algorithms.linear-algebra.gaussian-elimination-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.linear-algebra.gaussian-elimination
             :refer [gaussian-elimination retroactive-resolution]]))

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-retroactive-resolution
  (is (= [2.0 2.0 3.0]
         (gaussian-elimination [[2 2 -1] [0 -2 -1] [0 0 5]] [5 -7 15])))
  (let [[x y] (gaussian-elimination [[2 2] [0 -2]] [-1 -1])]
    (is (approx x -1.0))
    (is (approx y 0.5))))

(deftest test-gaussian-elimination
  (let [[x y z] (gaussian-elimination [[1 -4 -2] [5 2 -2] [1 -1 0]] [-2 -3 4])]
    (is (approx x 2.3))
    (is (approx y -1.7))
    (is (approx z 5.55)))
  (let [[x y] (gaussian-elimination [[1 2] [5 2]] [5 5])]
    (is (approx x 0.0))
    (is (approx y 2.5)))
  (is (= [] (gaussian-elimination [[1 2 3] [4 5 6]] [1 2]))))
