(ns clojure-algorithms.digital-image-processing.filters.sobel-filter
  "Port of digital_image_processing/filters/sobel_filter.py.
  Returns [gradient-magnitude theta] over nested vectors."
  (:require [clojure-algorithms.digital-image-processing.filters.convolve
             :refer [img-convolve]]))

(def kernel-x [[-1 0 1] [-2 0 2] [-1 0 1]])
(def kernel-y [[1 2 1] [0 0 0] [-1 -2 -1]])

(defn- normalize [m]
  (let [mx (apply max (mapcat identity m))]
    (if (zero? mx)
      m
      (mapv (fn [row] (mapv #(/ (* % 255.0) mx) row)) m))))

(defn sobel-filter
  "Vector [gradient theta] of IMAGE."
  [image]
  (let [dst-x (normalize (mapv (fn [row] (mapv #(Math/abs (double %)) row))
                               (img-convolve image kernel-x)))
        dst-y (normalize (mapv (fn [row] (mapv #(Math/abs (double %)) row))
                               (img-convolve image kernel-y)))
        dst-xy (mapv (fn [rx ry]
                       (mapv (fn [x y] (Math/sqrt (+ (* x x) (* y y)))) rx ry))
                     dst-x dst-y)
        dst-xy (normalize dst-xy)
        dst (mapv (fn [row] (mapv int row)) dst-xy)
        theta (mapv (fn [rx ry]
                      (mapv (fn [x y] (Math/atan2 (double y) (double x)))
                            rx ry))
                    dst-x dst-y)]
    [dst theta]))
