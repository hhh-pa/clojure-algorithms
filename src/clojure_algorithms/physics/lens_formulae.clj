(ns clojure-algorithms.physics.lens-formulae
  "Thin-lens formulae relating focal length, image and object distances.

  1/f = 1/v - 1/u with the standard sign convention (object left of lens).")

(def ^:private err-msg
  "Invalid inputs. Enter non zero values with respect to the sign convention.")

(defn focal-length-of-lens
  "Returns the focal length (m) given object and image distances (m)."
  [object-distance-from-lens image-distance-from-lens]
  (when (or (zero? object-distance-from-lens) (zero? image-distance-from-lens))
    (throw (ex-info err-msg {})))
  (/ 1 (- (/ 1 image-distance-from-lens) (/ 1 object-distance-from-lens))))

(defn object-distance
  "Returns the object distance (m) given focal length and image distance (m)."
  [focal-length image-distance-from-lens]
  (when (or (zero? image-distance-from-lens) (zero? focal-length))
    (throw (ex-info err-msg {})))
  (/ 1 (- (/ 1 image-distance-from-lens) (/ 1 focal-length))))

(defn image-distance
  "Returns the image distance (m) given focal length and object distance (m)."
  [focal-length object-distance-from-lens]
  (when (or (zero? object-distance-from-lens) (zero? focal-length))
    (throw (ex-info err-msg {})))
  (/ 1 (+ (/ 1 object-distance-from-lens) (/ 1 focal-length))))
