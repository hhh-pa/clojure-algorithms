(ns clojure-algorithms.digital-image-processing.edge-detection.canny
  "Port of digital_image_processing/edge_detection/canny.py.
  Full Canny pipeline over nested vectors."
  (:require [clojure-algorithms.digital-image-processing.filters.convolve
             :refer [img-convolve]]
            [clojure-algorithms.digital-image-processing.filters.sobel-filter
             :refer [sobel-filter]]))

(def pi 180)

(defn gen-gaussian-kernel
  "K-SIZE x K-SIZE Gaussian kernel with SIGMA."
  [k-size sigma]
  (let [center (quot k-size 2)]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [px (- x center)
                          py (- y center)]
                      (* (/ 1 (* 2 Math/PI sigma))
                         (Math/exp (/ (- (+ (* px px) (* py py)))
                                      (* 2 sigma sigma))))))
                  (range k-size)))
          (range k-size))))

(defn suppress-non-maximum
  "Non-maximum suppression of SOBEL-GRAD along GRADIENT-DIRECTION."
  [image-shape gradient-direction sobel-grad]
  (let [[rows cols] image-shape
        eighth (/ pi 8)]
    (mapv (fn [row]
            (mapv (fn [col]
                    (if (or (zero? row) (zero? col)
                            (= row (dec rows)) (= col (dec cols)))
                      0.0
                      (let [direction (nth (nth gradient-direction row) col)
                            g (fn [r c] (nth (nth sobel-grad r) c))
                            center (g row col)
                            [a b]
                            (cond
                              (or (and (<= 0 direction) (< direction eighth))
                                  (and (<= (* 15 eighth) direction)
                                       (<= direction (* 2 pi)))
                                  (and (<= (* 7 eighth) direction)
                                       (<= direction (* 9 eighth))))
                              [(g row (dec col)) (g row (inc col))]
                              (or (and (<= eighth direction)
                                       (< direction (* 3 eighth)))
                                  (and (<= (* 9 eighth) direction)
                                       (< direction (* 11 eighth))))
                              [(g (inc row) (dec col)) (g (dec row) (inc col))]
                              (or (and (<= (* 3 eighth) direction)
                                       (< direction (* 5 eighth)))
                                  (and (<= (* 11 eighth) direction)
                                       (< direction (* 13 eighth))))
                              [(g (dec row) col) (g (inc row) col)]
                              :else
                              [(g (dec row) (dec col)) (g (inc row) (inc col))])]
                        (if (and (>= center a) (>= center b)) center 0.0))))
                  (range cols)))
          (range rows))))

(defn detect-high-low-threshold
  "Classify edges into STRONG/WEAK/suppressed (pure version)."
  [image-shape destination threshold-low threshold-high weak strong]
  (let [[rows cols] image-shape]
    (mapv (fn [row]
            (mapv (fn [col]
                    (let [v (nth (nth destination row) col)]
                      (if (or (zero? row) (zero? col)
                              (= row (dec rows)) (= col (dec cols)))
                        v
                        (cond
                          (>= v threshold-high) strong
                          (<= v threshold-low) 0
                          :else weak))))
                  (range cols)))
          (range rows))))

(defn track-edge
  "Promote weak pixels touching a 255 pixel (pure version)."
  [image-shape destination weak strong]
  (let [[rows cols] image-shape
        getv (fn [r c]
               (if (and (<= 0 r (dec rows)) (<= 0 c (dec cols)))
                 (nth (nth destination r) c)
                 0))]
    (mapv (fn [row]
            (mapv (fn [col]
                    (let [v (getv row col)]
                      (if (and (>= row 1) (>= col 1) (= v weak))
                        (if (some #(= 255 %)
                                  [(getv row (inc col)) (getv row (dec col))
                                   (getv (dec row) col) (getv (inc row) col)
                                   (getv (dec row) (dec col))
                                   (getv (inc row) (dec col))
                                   (getv (dec row) (inc col))
                                   (getv (inc row) (inc col))])
                          strong
                          0)
                        v)))
                  (range cols)))
          (range rows))))

(defn canny
  "Canny edge map of IMAGE."
  ([image] (canny image 15 30 128 255))
  ([image threshold-low threshold-high weak strong]
   (let [shape [(count image) (count (first image))]
         gaussian-out (img-convolve image (gen-gaussian-kernel 9 1.4))
         [sobel-grad sobel-theta] (sobel-filter gaussian-out)
         gradient-direction
         (mapv (fn [row]
                 (mapv #(+ pi (* % (/ 180 Math/PI))) row))
               sobel-theta)
         destination (suppress-non-maximum shape gradient-direction sobel-grad)
         destination (detect-high-low-threshold shape destination
                                                threshold-low threshold-high
                                                weak strong)]
     (track-edge shape destination weak strong))))
