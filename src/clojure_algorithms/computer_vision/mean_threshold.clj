(ns clojure-algorithms.computer-vision.mean-threshold
  "Mean thresholding over 2D grayscale vectors.")

(defn mean-threshold
  "Binarizes an image around its mean pixel value."
  [image]
  (let [pixels (apply concat image)
        mean (quot (reduce + pixels) (count pixels))]
    (mapv (fn [row] (mapv #(if (> % mean) 255 0) row)) image)))
