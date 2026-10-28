(ns clojure-algorithms.computer-vision.intensity-based-segmentation
  "Intensity threshold image segmentation over 2D vectors.")

(defn segment-image
  "Labels each pixel by the thresholds it exceeds."
  [image thresholds]
  (mapv (fn [row]
          (mapv (fn [px]
                  (count (filter #(> px %) thresholds)))
                row))
        image))
