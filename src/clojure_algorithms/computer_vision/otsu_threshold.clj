(ns clojure-algorithms.computer-vision.otsu-threshold
  "Otsu thresholding over 2D grayscale vectors.")

(defn- otsu-cutoff
  [pixels]
  (let [hist (reduce (fn [h px] (update h px (fnil inc 0)))
                     (vec (repeat 256 0)) pixels)
        total (count pixels)
        sum-total (reduce + (map-indexed * hist))]
    (loop [i 0 wb 0.0 sf 0.0 best 0 best-t 0]
      (if (>= i 256)
        best-t
        (let [wb (+ wb (nth hist i))]
          (if (zero? wb)
            (recur (inc i) wb sf best best-t)
            (let [wf (- total wb)]
              (if (zero? wf)
                best-t
                (let [sf (+ sf (* i (nth hist i)))
                      mb (/ sf wb)
                      mf (/ (- sum-total sf) wf)
                      between (* wb wf (let [d (- mb mf)] (* d d)))]
                  (if (> between best)
                    (recur (inc i) wb sf between i)
                    (recur (inc i) wb sf best best-t)))))))))))

(defn otsu-threshold
  "Binarizes an image with Otsu's method."
  [image]
  (let [t (otsu-cutoff (apply concat image))]
    (mapv (fn [row] (mapv #(if (> % t) 255 0) row)) image)))
