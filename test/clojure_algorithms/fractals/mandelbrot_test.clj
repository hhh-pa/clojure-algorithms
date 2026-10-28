(ns clojure-algorithms.fractals.mandelbrot-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.fractals.mandelbrot
             :refer [get-distance get-black-and-white-rgb get-color-coded-rgb
                     pixel-grid]]))

(deftest distance-test
  (is (= 1.0 (get-distance 0 0 50)))
  (is (< (Math/abs (- (get-distance 0.5 0.5 50) 0.061224489795918366)) 1e-12))
  (is (= 0.0 (get-distance 2 0 50))))

(deftest color-test
  (is (= [255 255 255] (get-black-and-white-rgb 0)))
  (is (= [255 255 255] (get-black-and-white-rgb 0.5)))
  (is (= [0 0 0] (get-black-and-white-rgb 1)))
  (is (= [255 0 0] (get-color-coded-rgb 0)))
  (is (= [0 255 255] (get-color-coded-rgb 0.5)))
  (is (= [0 0 0] (get-color-coded-rgb 1))))

(deftest grid-test
  (let [grid (pixel-grid {:image-width 4 :image-height 3 :max-step 10})]
    (is (= 3 (count grid)))
    (is (= 4 (count (first grid))))
    (is (every? (fn [[r g b]] (every? #(<= 0 % 255) [r g b]))
                (apply concat grid)))))
