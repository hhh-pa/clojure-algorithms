(ns clojure-algorithms.maths.bearing
  "Initial bearing (forward azimuth) between two geographic points.

Points are (latitude, longitude) in decimal degrees. Returns bearing in degrees
clockwise from true north in the range [0, 360).

Reference: https://en.wikipedia.org/wiki/Bearing_(navigation)")

(defn- as-float
  "Coerce v to a double the way Python's float() does for numbers and strings."
  [v]
  (cond
    (number? v) (double v)
    (string? v) (Double/parseDouble v)
    :else (throw (IllegalArgumentException. "not numeric"))))

(defn- parse-point
  "Parse a 2-element (latitude, longitude) point into [lat lon] doubles."
  [point]
  [(as-float (nth point 0)) (as-float (nth point 1))])

(defn initial-bearing
  "Compute the initial bearing from `origin` to `destination`, each a
  2-element (latitude, longitude) sequential in decimal degrees.
  Returns the bearing in degrees, clockwise from north in [0, 360).
  Throws :TypeError if inputs are not 2-tuples of numeric values, and
  :ValueError if the two points are identical (bearing undefined)."
  [origin destination]
  (let [[lat1 lon1] (try (parse-point origin)
                         (catch Exception e
                           (throw (ex-info "origin and destination must be 2-tuples of numeric values"
                                           {:type :TypeError}))))
        [lat2 lon2] (try (parse-point destination)
                         (catch Exception e
                           (throw (ex-info "origin and destination must be 2-tuples of numeric values"
                                           {:type :TypeError}))))]
    (when (and (= lat1 lat2) (= lon1 lon2))
      (throw (ex-info "origin and destination are the same point; bearing is undefined"
                      {:type :ValueError})))
    (let [phi1 (Math/toRadians lat1)
          phi2 (Math/toRadians lat2)
          delta-lambda (Math/toRadians (- lon2 lon1))
          x (* (Math/sin delta-lambda) (Math/cos phi2))
          y (- (* (Math/cos phi1) (Math/sin phi2))
               (* (Math/sin phi1) (Math/cos phi2) (Math/cos delta-lambda)))
          theta (Math/atan2 x y)]
      (mod (+ (Math/toDegrees theta) 360.0) 360.0))))
