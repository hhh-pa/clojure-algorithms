(ns clojure-algorithms.digital-image-processing.change-brightness
  "Port of digital_image_processing/change_brightness.py.
  Images are nested vectors (grayscale numbers or [r g b] channel
  vectors); pixels are clamped to [0, 255].")

(defn- clamp [v]
  (long (min 255 (max 0 (Math/round (double v))))))

(defn- brighten-pixel [level pixel]
  (if (sequential? pixel)
    (mapv #(clamp (+ 128 level (- % 128))) pixel)
    (clamp (+ 128 level (- pixel 128)))))

(defn change-brightness
  "New image with brightness shifted by LEVEL (-255 to 255)."
  [img level]
  (when-not (<= -255.0 level 255.0)
    (throw (ex-info "level must be between -255.0 (black) and 255.0 (white)"
                    {:type :value-error})))
  (mapv (fn [row] (mapv #(brighten-pixel level %) row)) img))
