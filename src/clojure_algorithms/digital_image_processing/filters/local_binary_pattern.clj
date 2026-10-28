(ns clojure-algorithms.digital-image-processing.filters.local-binary-pattern
  "Port of digital_image_processing/filters/local_binary_pattern.py.
  Border handling mirrors the numpy semantics: negative indices wrap,
  oversize indices (and nil centres) yield 0.
  (The Python except clause has an IndexError/TypeError mix that only
  runs under Python 3; behaviour is inferred from the code.)")

(def powers-of-two [1 2 4 8 16 32 64 128])

(defn- pixel-or
  "Pixel at [X Y], wrapping negatives and returning 0 past the far edge."
  [image x y]
  (let [h (count image)
        w (count (first image))]
    (cond
      (or (>= x h) (>= y w)) 0
      (or (neg? x) (neg? y)) (nth (nth image (mod x h)) (mod y w))
      :else (nth (nth image x) y))))

(defn get-neighbors-pixel
  "8 neighbour values of CENTER at (X, Y), or nil when CENTER is nil."
  [image x y center]
  (when-not (nil? center)
    [(pixel-or image (dec x) (inc y))
     (pixel-or image x (inc y))
     (pixel-or image (dec x) y)
     (pixel-or image (inc x) (inc y))
     (pixel-or image (inc x) y)
     (pixel-or image (inc x) (dec y))
     (pixel-or image x (dec y))
     (pixel-or image (dec x) (dec y))]))

(defn local-binary-value
  "Local binary pattern value of the pixel at (X, Y)."
  [image x y]
  (let [center (pixel-or image x y)
        neighbors (get-neighbors-pixel image x y center)]
    (reduce + (map * (map #(if (>= % center) 1 0) neighbors)
                   powers-of-two))))

(defn local-binary-pattern
  "LBP image of grayscale IMAGE."
  [image]
  (let [h (count image)
        w (count (first image))]
    (mapv (fn [i] (mapv (fn [j] (local-binary-value image i j))
                        (range w)))
          (range h))))
