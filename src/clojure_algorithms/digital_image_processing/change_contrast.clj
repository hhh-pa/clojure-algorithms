(ns clojure-algorithms.digital-image-processing.change-contrast
  "Port of digital_image_processing/change_contrast.py.
  Images are nested vectors; pixels are clamped to [0, 255].")

(defn contrast-factor
  "Contrast factor for LEVEL."
  [level]
  (/ (* 259.0 (+ level 255)) (* 255.0 (- 259 level))))

(defn- clamp [v]
  (long (min 255 (max 0 v))))

(defn- contrast-pixel [factor pixel]
  (if (sequential? pixel)
    (mapv #(clamp (long (+ 128 (* factor (- % 128))))) pixel)
    (clamp (long (+ 128 (* factor (- pixel 128)))))))

(defn change-contrast
  "New image with contrast adjusted by LEVEL."
  [img level]
  (let [factor (contrast-factor level)]
    (mapv (fn [row] (mapv #(contrast-pixel factor %) row)) img)))
