(ns clojure-algorithms.physics.mirror-formulae
  "Spherical-mirror formulae relating focal length, image and object distances.

  1/f = 1/v + 1/u with the standard sign convention (object left of mirror).")

(def ^:private err-msg
  "Invalid inputs. Enter non zero values with respect to the sign convention.")

(defn focal-length
  "Returns the focal length (m) given object and image distances (m)."
  [distance-of-object distance-of-image]
  (when (or (zero? distance-of-object) (zero? distance-of-image))
    (throw (ex-info err-msg {})))
  (/ 1 (+ (/ 1 distance-of-object) (/ 1 distance-of-image))))

(defn object-distance
  "Returns the object distance (m) given focal length and image distance (m)."
  [focal-length distance-of-image]
  (when (or (zero? distance-of-image) (zero? focal-length))
    (throw (ex-info err-msg {})))
  (/ 1 (- (/ 1 focal-length) (/ 1 distance-of-image))))

(defn image-distance
  "Returns the image distance (m) given focal length and object distance (m)."
  [focal-length distance-of-object]
  (when (or (zero? distance-of-object) (zero? focal-length))
    (throw (ex-info err-msg {})))
  (/ 1 (- (/ 1 focal-length) (/ 1 distance-of-object))))
