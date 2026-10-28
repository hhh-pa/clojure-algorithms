(ns clojure-algorithms.geodesy.haversine-distance
  "Great-circle distance between two lat/lon points via the Haversine formula.")

(def earth-radius
  "Mean Earth radius in metres."
  6371000)

(defn haversine-distance
  "Returns the great-circle distance in metres between (lat1, lon1) and
   (lat2, lon2), given in degrees."
  [lat1 lon1 lat2 lon2]
  (let [phi-1 (Math/toRadians lat1)
        phi-2 (Math/toRadians lat2)
        dphi (/ (- phi-2 phi-1) 2.0)
        dlambda (/ (- (Math/toRadians lon2) (Math/toRadians lon1)) 2.0)
        h (+ (* (Math/sin dphi) (Math/sin dphi))
             (* (Math/cos phi-1) (Math/cos phi-2)
                (Math/sin dlambda) (Math/sin dlambda)))]
    (* 2 earth-radius (Math/asin (Math/sqrt h)))))
