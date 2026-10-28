(ns clojure-algorithms.digital-image-processing.histogram-equalization.histogram-stretch
  "Port of the pure histogram-equalization logic from
  digital_image_processing/histogram_equalization/histogram_stretch.py.
  (File reading, plotting and GUI display are omitted.)
  Note the Python `rem` accumulator is always 0, so each mapping is
  exactly int(255 * cdf).")

(defn equalization-mapping
  "256-entry lookup table mapping old grey levels to stretched ones."
  [gray-img]
  (let [pixels (vec (mapcat identity gray-img))
        total (count pixels)
        hist (reduce (fn [h v] (update h v (fnil inc 0)))
                     (vec (repeat 256 0)) pixels)]
    (first
     (reduce (fn [[mapping cdf] i]
               (let [cdf (+ cdf (/ (nth hist i) total))]
                 [(conj mapping (int (* 255 cdf))) cdf]))
             [[] 0] (range 256)))))

(defn histogram-stretch
  "Histogram-equalized copy of grayscale GRAY-IMG."
  [gray-img]
  (let [mapping (equalization-mapping gray-img)]
    (mapv (fn [row] (mapv #(nth mapping %) row)) gray-img)))
