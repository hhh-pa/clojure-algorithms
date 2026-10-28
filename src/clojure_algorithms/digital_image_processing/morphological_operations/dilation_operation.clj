(ns clojure-algorithms.digital-image-processing.morphological-operations.dilation-operation
  "Port of digital_image_processing/morphological_operations/dilation_operation.py.
  Images are nested vectors; binary pixels are booleans.
  Padding copies the image at offset [kh-2, kw-2] like the numpy slice
  [kh-2:-1, kw-2:-1] (empty for degenerate kernels, yielding all-false).")

(defn rgb-to-gray
  "Gray image from an RGB image (BGR and RGB agree here up to channel
  order; uses the Python weights on channels 0/1/2)."
  [rgb]
  (mapv (fn [row]
          (mapv (fn [[c0 c1 c2]]
                  (+ (* 0.2989 c0) (* 0.5870 c1) (* 0.1140 c2)))
                row))
        rgb))

(defn gray-to-binary
  "Binary (boolean) image: true when 127 < v <= 255."
  [gray]
  (mapv (fn [row] (mapv #(and (> % 127) (<= % 255)) row)) gray))

(defn- pad-image [image kh kw]
  (let [h (count image)
        w (count (first image))
        ph (+ h kh -1)
        pw (+ w kw -1)
        sr (- kh 2)
        sc (- kw 2)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (if (and (>= sr 0) (>= sc 0)
                             (>= i sr) (< i (+ sr h))
                             (>= j sc) (< j (+ sc w)))
                      (if (nth (nth image (- i sr)) (- j sc)) 1 0)
                      0))
                  (range pw)))
          (range ph))))

(defn dilation
  "Dilated binary image with structuring element KERNEL."
  [image kernel]
  (let [h (count image)
        w (count (first image))
        kh (count kernel)
        kw (count (first kernel))
        padded (pad-image image kh kw)]
    (mapv (fn [y]
            (mapv (fn [x]
                    (pos? (reduce + (for [ki (range kh) kj (range kw)]
                                      (* (nth (nth kernel ki) kj)
                                         (nth (nth padded (+ y ki))
                                              (+ x kj)))))))
                  (range w)))
          (range h))))
