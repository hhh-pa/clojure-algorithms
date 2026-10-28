(ns clojure-algorithms.geodesy.lamberts-ellipsoidal-distance
  "Shortest surface distance between two lat/lon points on the WGS84
   ellipsoid via Lambert's formula for long lines."
  (:require [clojure-algorithms.geodesy.haversine-distance
             :refer [earth-radius haversine-distance]]))

(def axis-a "WGS84 semi-major axis in metres." 6378137.0)
(def axis-b "WGS84 semi-minor axis in metres." 6356752.314245)
(def equatorial-radius "Equatorial radius in metres." 6378137)

(defn lamberts-ellipsoidal-distance
  "Returns the ellipsoidal distance in metres between (lat1, lon1) and
   (lat2, lon2), given in degrees. Throws on out-of-range coordinates."
  [lat1 lon1 lat2 lon2]
  (when (or (not (<= -90 lat1 90)) (not (<= -90 lat2 90)))
    (throw (IllegalArgumentException. "Latitude must be between -90 and 90 degrees")))
  (when (or (not (<= -180 lon1 180)) (not (<= -180 lon2 180)))
    (throw (IllegalArgumentException. "Longitude must be between -180 and 180 degrees")))
  (let [f (/ (- axis-a axis-b) axis-a)
        b-lat1 (Math/atan (* (- 1 f) (Math/tan (Math/toRadians lat1))))
        b-lat2 (Math/atan (* (- 1 f) (Math/tan (Math/toRadians lat2))))
        sigma (/ (haversine-distance lat1 lon1 lat2 lon2) earth-radius)
        p (/ (+ b-lat1 b-lat2) 2.0)
        q (/ (- b-lat2 b-lat1) 2.0)
        x-num (* (Math/pow (Math/sin p) 2) (Math/pow (Math/cos q) 2))
        x-den (Math/pow (Math/cos (/ sigma 2.0)) 2)
        x (* (- sigma (Math/sin sigma)) (/ x-num x-den))
        y-num (* (Math/pow (Math/cos p) 2) (Math/pow (Math/sin q) 2))
        y-den (Math/pow (Math/sin (/ sigma 2.0)) 2)
        y (* (+ sigma (Math/sin sigma)) (/ y-num y-den))]
    (* equatorial-radius (- sigma (* (/ f 2.0) (+ x y))))))
