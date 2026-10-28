(ns clojure-algorithms.digital-image-processing.filters.median-filter
  "Port of digital_image_processing/filters/median_filter.py.
  Same-size output with zero borders. The median index follows the
  Python version exactly: int(mask*mask/2 + 1).")

(defn median-filter
  "Median-filtered image with MASK x MASK window (default 3)."
  ([gray-img] (median-filter gray-img 3))
  ([gray-img mask]
   (let [h (count gray-img)
         w (count (first gray-img))
         bd (int (/ mask 2))
         idx (int (+ (/ (* mask mask) 2) 1))]
     (mapv (fn [i]
             (mapv (fn [j]
                     (if (or (< i bd) (< j bd)
                             (>= i (- h bd)) (>= j (- w bd)))
                       0
                       (nth (sort (for [ki (range (- i bd) (+ i bd 1))
                                        kj (range (- j bd) (+ j bd 1))]
                                    (nth (nth gray-img ki) kj)))
                            idx)))
                   (range w)))
           (range h)))))
