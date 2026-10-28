(ns clojure-algorithms.digital-image-processing.convert-to-negative
  "Port of digital_image_processing/convert_to_negative.py.
  Pure version returning a new image (the Python version mutates).")

(defn- negate-pixel [pixel]
  (if (sequential? pixel)
    (mapv #(- 255 %) pixel)
    (- 255 pixel)))

(defn convert-to-negative
  "New image with every channel negated (255 - v)."
  [img]
  (mapv (fn [row] (mapv negate-pixel row)) img))
