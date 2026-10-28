(ns clojure-algorithms.digital-image-processing.sepia
  "Port of digital_image_processing/sepia.py.
  Pixels are [blue green red] triples (cv2 order).
  Pure version returning a new image.")

(defn to-grayscale
  "Grayscale value of a BGR pixel."
  [blue green red]
  (+ (* 0.2126 red) (* 0.587 green) (* 0.114 blue)))

(defn- normalize [value]
  (min value 255))

(defn make-sepia
  "New image with sepia tone of FACTOR."
  [img factor]
  (mapv (fn [row]
          (mapv (fn [[blue green red]]
                  (let [grey (int (to-grayscale blue green red))]
                    [(normalize grey)
                     (normalize (+ grey factor))
                     (normalize (+ grey (* 2 factor)))]))
                row))
        img))
