(ns clojure-algorithms.digital-image-processing.morphological-operations.erosion-operation
  "Port of digital_image_processing/morphological_operations/erosion_operation.py.
  Images are nested vectors; binary pixels are booleans.")

(defn rgb-to-gray
  "Gray image from an RGB image."
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

(defn erosion
  "Eroded binary image with structuring element KERNEL.
  (Keeps the Python quirk of comparing the kernel sum to 5.)"
  [image kernel]
  (let [h (count image)
        w (count (first image))
        kh (count kernel)
        kw (count (first kernel))
        padded (pad-image image kh kw)]
    (mapv (fn [y]
            (mapv (fn [x]
                    (= 5 (reduce + (for [ki (range kh) kj (range kw)]
                                     (* (nth (nth kernel ki) kj)
                                        (nth (nth padded (+ y ki))
                                             (+ x kj)))))))
                  (range w)))
          (range h))))
