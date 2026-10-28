(ns clojure-algorithms.digital-image-processing.filters.convolve
  "Port of digital_image_processing/filters/convolve.py.
  Same-size convolution with edge padding over nested vectors.
  (The Python im2col swaps height/width names, which only works for
  square inputs; this port is shape-correct for all inputs and
  identical on square ones.)")

(defn- edge-pad [image pad-size]
  (let [h (count image)
        w (count (first image))
        getv (fn [i j]
               (nth (nth image (min (dec h) (max 0 i)))
                    (min (dec w) (max 0 j))))]
    (mapv (fn [i] (mapv (fn [j] (getv (- i pad-size) (- j pad-size)))
                        (range (+ w (* 2 pad-size)))))
          (range (+ h (* 2 pad-size))))))

(defn img-convolve
  "Convolve IMAGE with square odd-sized FILTER-KERNEL (edge-padded,
  same-size output)."
  [image filter-kernel]
  (let [h (count image)
        w (count (first image))
        k (count filter-kernel)
        pad-size (quot k 2)
        padded (edge-pad image pad-size)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce + (for [ki (range k) kj (range k)]
                                (* (nth (nth padded (+ i ki)) (+ j kj))
                                   (nth (nth filter-kernel ki) kj)))))
                  (range w)))
          (range h))))
