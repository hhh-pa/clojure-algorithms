(ns clojure-algorithms.geodesy.radar-target-calculation
  "Geodetic/ECEF/ENU coordinate conversions and radar target positioning
   on the WGS84 ellipsoid.")

(def wgs84-a "WGS84 semi-major axis in metres." 6378137.0)
(def wgs84-b "WGS84 semi-minor axis in metres." 6356752.314245)
(def wgs84-e-sq "First eccentricity squared."
  (- 1.0 (/ (* wgs84-b wgs84-b) (* wgs84-a wgs84-a))))
(def wgs84-ep-sq "Second eccentricity squared."
  (/ (- (* wgs84-a wgs84-a) (* wgs84-b wgs84-b)) (* wgs84-b wgs84-b)))

(defn geodetic-to-ecef
  "Converts geodetic (lat, lon in degrees, alt in metres) to ECEF [x y z]."
  [lat-deg lon-deg alt-m]
  (let [lat (Math/toRadians lat-deg)
        lon (Math/toRadians lon-deg)
        sin-lat (Math/sin lat)
        n (/ wgs84-a (Math/sqrt (- 1.0 (* wgs84-e-sq sin-lat sin-lat))))]
    [(* (+ n alt-m) (Math/cos lat) (Math/cos lon))
     (* (+ n alt-m) (Math/cos lat) (Math/sin lon))
     (* (+ (* n (- 1.0 wgs84-e-sq)) alt-m) sin-lat)]))

(defn ecef-to-geodetic
  "Converts ECEF [x y z] to geodetic [lat lon alt] via Bowring's method."
  [x y z]
  (let [p (Math/sqrt (+ (* x x) (* y y)))]
    (if (zero? p)
      [(if (pos? z) 90.0 -90.0) 0.0 (- (Math/abs z) wgs84-b)]
      (let [theta (Math/atan2 (* z wgs84-a) (* p wgs84-b))
            st (Math/sin theta)
            ct (Math/cos theta)
            lat (Math/atan2 (+ z (* wgs84-ep-sq wgs84-b st st st))
                            (- p (* wgs84-e-sq wgs84-a ct ct ct)))
            lon (Math/atan2 y x)
            sl (Math/sin lat)
            n (/ wgs84-a (Math/sqrt (- 1.0 (* wgs84-e-sq sl sl))))]
        [(Math/toDegrees lat) (Math/toDegrees lon)
         (- (/ p (Math/cos lat)) n)]))))

(defn enu-to-ecef
  "Rotates local ENU offsets to ECEF offsets about reference lat/lon."
  [east north up ref-lat-deg ref-lon-deg]
  (let [lat (Math/toRadians ref-lat-deg)
        lon (Math/toRadians ref-lon-deg)
        sl (Math/sin lat) cl (Math/cos lat)
        so (Math/sin lon) co (Math/cos lon)]
    [(- (* (- sl) co north) (* so east) (- (* cl co up)))
     (+ (* co east) (* (- sl) so north) (* cl so up))
     (+ (* cl north) (* sl up))]))

(defn calculate-target-coordinates
  "Returns [lat lon alt] of a target seen from radar position (lat, lon,
   alt) at azimuth/range (metres) and optional elevation angle in degrees."
  ([radar-lat radar-lon radar-alt azimuth-deg range-m]
   (calculate-target-coordinates radar-lat radar-lon radar-alt
                                 azimuth-deg range-m 0.0))
  ([radar-lat radar-lon radar-alt azimuth-deg range-m elevation-deg]
   (let [az (Math/toRadians azimuth-deg)
         el (Math/toRadians elevation-deg)
         east (* range-m (Math/cos el) (Math/sin az))
         north (* range-m (Math/cos el) (Math/cos az))
         up (* range-m (Math/sin el))
         [rx ry rz] (geodetic-to-ecef radar-lat radar-lon radar-alt)
         [dx dy dz] (enu-to-ecef east north up radar-lat radar-lon)]
     (ecef-to-geodetic (+ rx dx) (+ ry dy) (+ rz dz)))))
