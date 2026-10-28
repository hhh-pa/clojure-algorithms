(ns clojure-algorithms.fractals.vicsek-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.vicsek
             :refer [cross-polygon fractal-centers vicsek-fractal]]))

(deftest cross-test
  (let [poly (cross-polygon 0 0 6)]
    (is (= 12 (count poly)))
    (is (every? (fn [[px py]] (and (<= -3.0 px 3.0) (<= -3.0 py 3.0))) poly))))

(deftest centers-test
  (is (= [[0.0 0.0 9.0]] (fractal-centers 0 0 9 0)))
  (is (= 5 (count (fractal-centers 0 0 9 1))))
  (is (= 25 (count (fractal-centers 0 0 9 2))))
  (is (= 125 (count (fractal-centers 0 0 9 3)))))

(deftest fractal-test
  (let [shapes (vicsek-fractal 0 0 9 1)]
    (is (= 5 (count shapes)))
    (is (every? #(= 12 (count %)) shapes))))
